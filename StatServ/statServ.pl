#!/usr/bin/perl -w
#
# SpringRTS lobby statistics service bot. This bot monitors the lobby and
# computes various statistics in real time. The statistics should be polled
# regularly using StatClient.
#
# Copyright (C) 2008-2013  Yann Riou <yaribzh@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

use strict;

use POSIX ":sys_wait_h";
use IO::Select;
use Storable "dclone";
use Time::HiRes;

use SimpleLog;
use StatServConf;
use SpringLobbyInterface;

my $statServVer='0.3';

my $win=$^O eq 'MSWin32' ? 1 : 0;

my %botCommandHandlers = (
                          dumpstatus => \&hDumpStatus,
                          help => \&hHelp,
                          helpall => \&hHelpAll,
                          monitor => \&hMonitor,
                          quit => \&hQuit,
                          restart => \&hRestart,
                          version => \&hVersion
                          );

# Basic checks ################################################################

if($#ARGV != 0 || ! (-f $ARGV[0])) {
  print "usage: $0 <configurationFile>\n";
  exit 1;
}

my $confFile=$ARGV[0];
my $sLog=SimpleLog->new(prefix => "[StatServ] ");
my $botConf=StatServConf->new($confFile,$sLog);

sub slog {
  $sLog->log(@_);
}

if(! $botConf) {
  slog("Unable to load StatServ configuration at startup",0);
  exit 1;
}

# State variables #############################################################

my %conf=%{$botConf->{conf}};
my $lSock;
my @sockets=();
my $running=1;
my $quitScheduled=0;
my %timestamps=(connectAttempt => 0,
                ping => 0,
                pong => 0);

my $lobbyState=0; # (0:not_connected, 1:connecting, 2: connected, 3:logged_in, 4:start_data_received)
my $p_answerFunction;
my %lastSentMessages;
my @messageQueue=();
my @lowPriorityMessageQueue=();

my $lobbySimpleLog=SimpleLog->new(logFiles => [$conf{logDir}."/statServ.log"],
                                  logLevels => [$conf{lobbyInterfaceLogLevel}],
                                  useANSICodes => [0],
                                  useTimestamps => [1],
                                  prefix => "[SpringLobbyInterface] ");

my $lobby = SpringLobbyInterface->new(serverHost => $conf{lobbyHost},
                                      serverPort => $conf{lobbyPort},
                                      simpleLog => $lobbySimpleLog,
                                      warnForUnhandledMessages => 0);

$ENV{LANG}="C";

# StatServ specific variables #################################################

my %reportData = (ranks => [0..7],
                  countries => [qw/AU BR CA CZ DE FI FR GB IT NL PL RU SE US/],
                  lobbyStates => [qw/inactive waiting playing/],
                  battleStates => [qw/empty waiting playing/],
                  nbPlayers => [1,2,8,12,16],
                  mods => {zk => ['Zero-K'],
                           ba => ['Balanced Annihilation','BA Chicken Defense'],
                           ta => ['Tech Annihilation','Robot Defense'],
                           nota => ['NOTA'],
                           xta => ['XTA'],
                           evo => ['Evolution RTS']},
                  decoTypes => { 'Quit' => 'normal',
                                 'Quit: Exiting' => 'normal',
                                 'renaming' => 'rename',
                                 'Ghosted' => 'timeout',
                                 'Connection timed out' => 'timeout',
                                 'Kicked for flooding' => 'kicked',
                                 'Kicked from server' => 'kicked'} );

my @stats=qw/rank botRank country botCountry state botState pSize pMod bSize bMod network/;
my %statsFiles;

foreach my $statType (@stats) {
  $statsFiles{$statType}="$conf{varDir}/${statType}.stats";
}

my %lastReportTimes;

my %cLastComputeTimes;
my %bLastComputeTimes;

my %pTimeByRank;
my %pTimeByCountry;
my %pTimeByState;

my %bTimeByRank;
my %bTimeByCountry;
my %bTimeByState;

my %pTimeByBState_Size;
my %pTimeByBState_Mod;

my %bTimeByBState_Size;
my %bTimeByBState_Mod;

my %gNbDecoByType;
my %quitReasons;
my $pingCount;
my $pingTime;

my %users;
my %battles;

my %onlineMonitor;
my %endgameMonitor;
my %activeMonitor;

# Subfunctions ################################################################

sub forkedError {
  my ($msg,$level)=@_;
  slog($msg,$level);
  exit 1;
}

sub getCpuSpeed {
  return 0 if($win);
  if(-f "/proc/cpuinfo" && -r "/proc/cpuinfo") {
    my @cpuInfo=`cat /proc/cpuinfo 2>/dev/null`;
    my %cpu;
    foreach my $line (@cpuInfo) {
      if($line =~ /^([\w\s]*\w)\s*:\s*(.*)$/) {
        $cpu{$1}=$2;
      }
    }
    if(defined $cpu{"model name"} && $cpu{"model name"} =~ /(\d+)\+/) {
      return $1;
    }
    if(defined $cpu{"cpu MHz"} && $cpu{"cpu MHz"} =~ /^(\d+)(?:\.\d*)?$/) {
      return $1;
    }
    if(defined $cpu{bogomips} && $cpu{bogomips} =~ /^(\d+)(?:\.\d*)?$/) {
      return $1;
    }
    slog("Unable to parse CPU info from /proc/cpuinfo",2);
    return 0;
  }else{
    slog("Unable to retrieve CPU info from /proc/cpuinfo",2);
    return 0;
  }
}

sub getLocalLanIp {
  return '*' if($win);
  my @ips;

  my $ifconfigBin;
  if(-x '/sbin/ifconfig') {
    $ifconfigBin='/sbin/ifconfig';
  }elsif(-x '/bin/ifconfig') {
    $ifconfigBin='/bin/ifconfig';
  }else{
    $ifconfigBin='ifconfig';
  }
  my @ifConfOut=`$ifconfigBin`;
  foreach my $line (@ifConfOut) {
    next unless($line =~ /inet addr:\s*(\d+\.\d+\.\d+\.\d+)\s/);
    push(@ips,$1);
  }
  foreach my $ip (@ips) {
    if($ip =~ /^10\./ || $ip =~ /192\.168\./) {
      slog("Following local LAN IP address detected: $ip",4);
      return $ip;
    }
    if($ip =~ /^172\.(\d+)\./) {
      if($1 > 15 && $1 < 32) {
        slog("Following local LAN IP address detected: $ip",4);
        return $ip;
      }
    }
  }
  slog("No local LAN IP address found",4);
  return "*";
}

sub scheduleQuit {
  my $reason=shift;
  $quitScheduled=1;
  my $msg="Bot shutdown scheduled (reason: $reason)";
  broadcastMsg($msg);
  slog($msg,3);
}

sub scheduleRestart {
  my $reason=shift;
  $quitScheduled=2;
  my $msg="Bot restart scheduled (reason: $reason)";
  broadcastMsg($msg);
  slog($msg,3);
}

sub computeMessageSize {
  my $p_msg=shift;
  my $size=0;
  {
    use bytes;
    foreach my $word (@{$p_msg}) {
      $size+=length($word)+1;
    }
  }
  return $size;
}

sub checkLastSentMessages {
  my $sent=0;
  foreach my $timestamp (keys %lastSentMessages) {
    if(time - $timestamp > $conf{sendRecordPeriod}) {
      delete $lastSentMessages{$timestamp};
    }else{
      foreach my $msgSize (@{$lastSentMessages{$timestamp}}) {
        $sent+=$msgSize;
      }
    }
  }
  return $sent;
}

sub queueLobbyCommand {
  my @params=@_;
  if($params[0]->[0] =~ /SAYPRIVATE/) {
    if(@lowPriorityMessageQueue) {
      push(@lowPriorityMessageQueue,\@params);
    }else{
      my $alreadySent=checkLastSentMessages();
      my $toBeSent=computeMessageSize($params[0]);
      if($alreadySent+$toBeSent+5 >= $conf{maxLowPrioBytesSent}) {
        slog("Output flood protection: queueing low priority message(s)",3);
        push(@lowPriorityMessageQueue,\@params);
      }else{
        sendLobbyCommand(\@params,$toBeSent);
      }
    }
  }elsif(@messageQueue) {
    push(@messageQueue,\@params);
  }else{
    my $alreadySent=checkLastSentMessages();
    my $toBeSent=computeMessageSize($params[0]);
    if($alreadySent+$toBeSent+5 >= $conf{maxBytesSent}) {
      slog("Output flood protection: queueing message(s)",2);
      push(@messageQueue,\@params);
    }else{
      sendLobbyCommand(\@params,$toBeSent);
    }
  }
}

sub sendLobbyCommand {
  my ($p_params,$size)=@_;
  $size=computeMessageSize($p_params->[0]) unless(defined $size);
  my $timestamp=time;
  $lastSentMessages{$timestamp}=[] unless(exists $lastSentMessages{$timestamp});
  push(@{$lastSentMessages{$timestamp}},$size);
  $lobby->sendCommand(@{$p_params});
}

sub checkQueuedLobbyCommands {
  return unless($lobbyState > 1 && (@messageQueue || @lowPriorityMessageQueue));
  my $alreadySent=checkLastSentMessages();
  while(@messageQueue) {
    my $toBeSent=computeMessageSize($messageQueue[0]->[0]);
    last if($alreadySent+$toBeSent+5 >= $conf{maxBytesSent});
    my $p_command=shift(@messageQueue);
    sendLobbyCommand($p_command,$toBeSent);
    $alreadySent+=$toBeSent;
  }
  while(@lowPriorityMessageQueue) {
    my $toBeSent=computeMessageSize($lowPriorityMessageQueue[0]->[0]);
    last if($alreadySent+$toBeSent+5 >= $conf{maxLowPrioBytesSent});
    my $p_command=shift(@lowPriorityMessageQueue);
    sendLobbyCommand($p_command,$toBeSent);
    $alreadySent+=$toBeSent;
  }
}

sub checkTimedEvents {
  if($lobbyState >= 4 && time - $timestamps{connectAttempt} > 60) {
    generateNetworkReport() unless(-f $statsFiles{network});
    my $firstClientReport=1;
    my $firstBattleReport=1;
    my %clientReports=($statsFiles{rank} => \&generateRankReport,
                       $statsFiles{botRank} => \&generateBotRankReport,
                       $statsFiles{country} => \&generateCountryReport,
                       $statsFiles{botCountry} => \&generateBotCountryReport,
                       $statsFiles{state} => \&generateStateReport,
                       $statsFiles{botState} => \&generateBotStateReport,
                       $statsFiles{pSize} => \&generatePSizeReport,
                       $statsFiles{pMod} => \&generatePModReport);
    my %battleReports=($statsFiles{bSize} => \&generateBSizeReport,
                       $statsFiles{bMod} => \&generateBModReport);
    foreach my $clientFile (keys %clientReports) {
      if(! -f $clientFile) {
        if($firstClientReport) {
          computeAllUsers();
          $firstClientReport=0;
        }
        &{$clientReports{$clientFile}}();
      }
    }
    foreach my $battleFile (keys %battleReports) {
      if(! -f $battleFile) {
        if($firstBattleReport) {
          computeAllBattles();
          $firstBattleReport=0;
        }
        &{$battleReports{$battleFile}}();
      }
    }
  }
}

sub answer {
  my $msg=shift;
  &{$p_answerFunction}($msg);
}

sub broadcastMsg {
  my $msg=shift;
  sayChan($conf{masterChannel},$msg) if($lobbyState >= 4 && (exists $lobby->{channels}->{$conf{masterChannel}}));
}

sub splitMsg {
  my ($longMsg,$maxSize)=@_;
  my @messages=($longMsg =~ /.{1,$maxSize}/gs);
  return \@messages;
}

sub sayPrivate {
  my ($user,$msg)=@_;
  my $p_messages=splitMsg($msg,$conf{maxChatMessageLength}-1);
  foreach my $mes (@{$p_messages}) {
    queueLobbyCommand(["SAYPRIVATE",$user,$mes]);
    logMsg("pv_$user","<$conf{lobbyLogin}> $mes") if($conf{logPvChat});
  }
}

sub sayChan {
  my ($chan,$msg)=@_;
  my $p_messages=splitMsg($msg,$conf{maxChatMessageLength}-3);
  foreach my $mes (@{$p_messages}) {
    queueLobbyCommand(["SAYEX",$chan,"* $mes"]);
  }
}

sub getCommandLevels {
  my ($source,$user,$cmd)=@_;
  return $botConf->getCommandLevels($cmd,$source,"outside","stopped");

}

sub getUserAccessLevel {
  my $user=shift;
  my $p_userData;
  if(! exists $lobby->{users}->{$user}) {
    return 0;
  }else{
    $p_userData=$lobby->{users}->{$user};
  }
  return $botConf->getUserAccessLevel($user,$p_userData);
}

sub handleRequest {
  my ($source,$user,$command)=@_;

  my @cmd=split(/ /,$command);
  my $lcCmd=lc($cmd[0]);

  my %answerFunctions = ( pv => sub { sayPrivate($user,$_[0]) },
                          chan => sub { sayChan($conf{masterChannel},$_[0]) } );
  $p_answerFunction=$answerFunctions{$source};

  if(exists $botConf->{commands}->{$lcCmd}) {

    my $p_levels=getCommandLevels($source,$user,$lcCmd);
    my $level=getUserAccessLevel($user);

    if(defined $p_levels->{directLevel} && $p_levels->{directLevel} ne '' && $level >= $p_levels->{directLevel}) {
      executeCommand($source,$user,\@cmd);
    }else{
      answer("$user, you are not allowed to call command \"$cmd[0]\" in current context.");
    }

  }else{
    answer("$user, \"$cmd[0]\" is not a valid command.") unless($source eq 'chan');
  }
}

sub executeCommand {
  my ($source,$user,$p_cmd)=@_;

  my %answerFunctions = ( pv => sub { sayPrivate($user,$_[0]) },
                          chan => sub { sayChan($conf{masterChannel},$_[0]) } );
  $p_answerFunction=$answerFunctions{$source};

  my @cmd=@{$p_cmd};
  my $command=lc(shift(@cmd));

  if(exists $botCommandHandlers{$command}) {
    return &{$botCommandHandlers{$command}}($source,$user,\@cmd);
  }else{
    answer("$user, \"$command\" is not a valid command.");
    return 0;
  }

}

sub invalidSyntax {
  my ($user,$cmd,$reason)=@_;
  $reason='' unless(defined $reason);
  $reason=" (".$reason.")" if($reason);
  answer("Invalid $cmd command usage$reason. $user, please refer to help sent in private message.");
  executeCommand("pv",$user,["help",$cmd]);
}

sub logMsg {
  my ($file,$msg)=@_;
  if(! -d $conf{logDir}."/chat") {
    if(! mkdir($conf{logDir}."/chat")) {
      slog("Unable to create directory \"$conf{logDir}/chat\"",1);
      return;
    }
  }
  if(! open(CHAT,">>$conf{logDir}/chat/$file.log")) {
    slog("Unable to log chat message into file \"$conf{logDir}/chat/$file.log\"",1);
    return;
  }
  my $dateTime=localtime();
  print CHAT "[$dateTime] $msg\n";
  close(CHAT);
}

sub computeUser {
  my $user=shift;
  my $currentTime=time;

  if(! exists $users{$user}) {
    slog("Unable to compute user stats for \"$user\", no data available!",2);
    $cLastComputeTimes{$user}=$currentTime;
    return;
  }

  return if($lobbyState < 4);
  my $delta=$currentTime-$cLastComputeTimes{$user};
  
  my $battleId=$users{$user}->{battle};

  my $rank=$users{$user}->{status}->{rank};
  if(! defined $rank) {
    slog("Undefined rank for user \"$user\"",2);
    $rank=0;
  }elsif(! (grep {/^$rank$/} @{$reportData{ranks}})) {
    slog("Invalid rank for user \"$user\": $rank",2);
    $rank=0;
  }
  
  my $country=$users{$user}->{country};
  if(! defined $country) {
    slog("Undefined country for user \"$user\"",2);
    $country='_OTHER_';
  }elsif(! (grep {/^$country$/} @{$reportData{countries}})) {
    $country='_OTHER_';
  }

  my $status='inactive';

  my $nbPlayers;
  my $mod;

  if(defined $battleId) {

    my $realNbPlayers;
    if($users{$user}->{status}->{inGame} && $users{$battles{$battleId}->{founder}}->{status}->{inGame} && (defined $battles{$battleId}->{nbStartPlayers})) {
      $status='playing';
      $realNbPlayers=$battles{$battleId}->{nbStartPlayers};
    }else{
      $status='waiting';
      $realNbPlayers=$battles{$battleId}->{maxPlayers};
    }

    $mod='_OTHER_';
    foreach my $modName (keys %{$reportData{mods}}) {
      foreach my $modRegex (@{$reportData{mods}->{$modName}}) {
        if($battles{$battleId}->{mod} =~ /^$modRegex/) {
          $mod=$modName;
          last;
        }
      }
      last unless($mod eq '_OTHER_');
    }

    $nbPlayers='_OTHER_';
    for my $i (0..$#{$reportData{nbPlayers}}) {
      next if($realNbPlayers > $reportData{nbPlayers}->[$i]);
      $nbPlayers=$reportData{nbPlayers}->[$i];
      last;
    }

  }

  if($users{$user}->{status}->{bot}) {
    $bTimeByRank{$rank}+=$delta;
    $bTimeByCountry{$country}+=$delta;
    $bTimeByState{$status}+=$delta;
  }else{
    $pTimeByRank{$rank}+=$delta;
    $pTimeByCountry{$country}+=$delta;
    $pTimeByState{$status}+=$delta;
    
    if($status ne 'inactive') {
      $pTimeByBState_Size{$status}->{$nbPlayers}+=$delta;
      $pTimeByBState_Mod{$status}->{$mod}+=$delta;
    }
  }

  $cLastComputeTimes{$user}=$currentTime;
}

sub computeBattle {
  my $battleId=shift;
  my $currentTime=time;
  return if($lobbyState < 4);
  my $delta=$currentTime-$bLastComputeTimes{$battleId};

  if(! exists $battles{$battleId}) {
    slog("Unable to compute battle stats for battle \"$battleId\", no data available!",2);
    $bLastComputeTimes{$battleId}=$currentTime;
    return;
  }
  if(! (exists $battles{$battleId}->{founder} && defined $battles{$battleId}->{founder}) ) {
    slog("Unable to compute battle stats for battle \"$battleId\", founder not defined!",2);
    $bLastComputeTimes{$battleId}=$currentTime;
    return;
  }
  if(! (exists $users{$battles{$battleId}->{founder}}->{status} && defined $users{$battles{$battleId}->{founder}}->{status})) {
    slog("Unable to compute battle stats for battle \"$battleId\", founder status not defined!",2);
    $bLastComputeTimes{$battleId}=$currentTime;
    return;
  }

  my $status;
  my $realNbPlayers;
  if($users{$battles{$battleId}->{founder}}->{status}->{inGame} && (defined $battles{$battleId}->{nbStartPlayers})) {
    $status='playing';
    $realNbPlayers=$battles{$battleId}->{nbStartPlayers};
  }else{
    $realNbPlayers=$battles{$battleId}->{maxPlayers};
    if($#{$battles{$battleId}->{userList}}+1-$battles{$battleId}->{nbSpec} < 1) {
      $status='empty';
    }else{
      $status='waiting';
    }
  }

  if(!defined $realNbPlayers) {
    $realNbPlayers=0;
    slog("Unable to find realNbPlayers for battle \"$battleId\"",2);
  }

  my $mod='_OTHER_';
  if(defined $battles{$battleId}->{mod}) {
    foreach my $modName (keys %{$reportData{mods}}) {
      foreach my $modRegex (@{$reportData{mods}->{$modName}}) {
        if($battles{$battleId}->{mod} =~ /^$modRegex/) {
          $mod=$modName;
          last;
        }
      }
      last unless($mod eq '_OTHER_');
    }
  }else{
    slog("Undefined mod for battle \"$battleId\"",2);
  }
  
  my $nbPlayers='_OTHER_';
  for my $i (0..$#{$reportData{nbPlayers}}) {
    next if($realNbPlayers > $reportData{nbPlayers}->[$i]);
    $nbPlayers=$reportData{nbPlayers}->[$i];
    last;
  }
  
  $bTimeByBState_Size{$status}->{$nbPlayers}+=$delta;
  $bTimeByBState_Mod{$status}->{$mod}+=$delta;

  $bLastComputeTimes{$battleId}=$currentTime;
}

sub computeAllUsers {
  foreach my $user (keys %users) {
    computeUser($user);
  }
}

sub computeAllBattles {
  foreach my $battleId (keys %battles) {
    computeBattle($battleId);
  }
}

sub initAllStats {
  initRankStats();
  initBotRankStats();
  initCountryStats();
  initBotCountryStats();
  initStateStats();
  initBotStateStats();
  initPSizeStats();
  initPModStats();
  initBSizeStats();
  initBModStats();
  initNetworkStats();
}

sub initRankStats {
  foreach my $rank (@{$reportData{ranks}}) {
    $pTimeByRank{$rank}=0;
  }
  $lastReportTimes{rank}=time;
}

sub initBotRankStats {
  foreach my $rank (@{$reportData{ranks}}) {
    $bTimeByRank{$rank}=0;
  }
  $lastReportTimes{botRank}=time;
}

sub initCountryStats {
  foreach my $country (@{$reportData{countries}},'_OTHER_') {
    $pTimeByCountry{$country}=0;
  }
  $lastReportTimes{country}=time;
}

sub initBotCountryStats {
  foreach my $country (@{$reportData{countries}},'_OTHER_') {
    $bTimeByCountry{$country}=0;
  }
  $lastReportTimes{botCountry}=time;
}

sub initStateStats {
  foreach my $state (@{$reportData{lobbyStates}}) {
    $pTimeByState{$state}=0;
  }
  $lastReportTimes{state}=time;
}

sub initBotStateStats {
  foreach my $state (@{$reportData{lobbyStates}}) {
    $bTimeByState{$state}=0;
  }
  $lastReportTimes{botState}=time;
}

sub initPSizeStats {
  foreach my $state (@{$reportData{battleStates}}) {
    foreach my $players (@{$reportData{nbPlayers}},'_OTHER_') {
      $pTimeByBState_Size{$state}->{$players}=0;
    }
  }
  $lastReportTimes{pSize}=time;
}

sub initPModStats {
  foreach my $state (@{$reportData{battleStates}}) {
    foreach my $mod (keys %{$reportData{mods}},'_OTHER_') {
      $pTimeByBState_Mod{$state}->{$mod}=0;
    }
  }
  $lastReportTimes{pMod}=time;
}

sub initBSizeStats {
  foreach my $state (@{$reportData{battleStates}}) {
    foreach my $players (@{$reportData{nbPlayers}},'_OTHER_') {
      $bTimeByBState_Size{$state}->{$players}=0;
    }
  }
  $lastReportTimes{bSize}=time;
}

sub initBModStats {
  foreach my $state (@{$reportData{battleStates}}) {
    foreach my $mod (keys %{$reportData{mods}},'_OTHER_') {
      $bTimeByBState_Mod{$state}->{$mod}=0;
    }
  }
  $lastReportTimes{bMod}=time;
}

sub initNetworkStats {
  foreach my $type ((values %{$reportData{decoTypes}}),'unknown','_OTHER_') {
    $gNbDecoByType{$type}=0;
  }
  $pingCount=0;
  $pingTime=0;
  $lastReportTimes{network}=time;
}

sub generateRankReport {
  my $output='';
  my $delta=time-$lastReportTimes{rank};
  $delta=1 if($delta == 0);
  foreach my $rank (@{$reportData{ranks}}) {
    $output.=" $rank:".($pTimeByRank{$rank} / $delta);
  }
  $output=~s/^ //;
  if(open(REPORT,">$statsFiles{rank}")) {
    print REPORT $output;
    close(REPORT);
  }else{
    slog("Unable to write to stat file $statsFiles{rank}",1);
  }
  initRankStats();
}

sub generateBotRankReport {
  my $output='';
  my $delta=time-$lastReportTimes{botRank};
  $delta=1 if($delta == 0);
  foreach my $rank (@{$reportData{ranks}}) {
    $output.=" $rank:".($bTimeByRank{$rank} / $delta);
  }
  $output=~s/^ //;
  if(open(REPORT,">$statsFiles{botRank}")) {
    print REPORT $output;
    close(REPORT);
  }else{
    slog("Unable to write to stat file $statsFiles{botRank}",1);
  }
  initBotRankStats();
}

sub generateCountryReport {
  my $output='';
  my $delta=time-$lastReportTimes{country};
  $delta=1 if($delta == 0);
  foreach my $country (@{$reportData{countries}},'_OTHER_') {
    $output.=" $country:".($pTimeByCountry{$country} / $delta);
  }
  $output=~s/^ //;
  if(open(REPORT,">$statsFiles{country}")) {
    print REPORT $output;
    close(REPORT);
  }else{
    slog("Unable to write to stat file $statsFiles{country}",1);
  }
  initCountryStats();
}

sub generateBotCountryReport {
  my $output='';
  my $delta=time-$lastReportTimes{botCountry};
  $delta=1 if($delta == 0);
  foreach my $country (@{$reportData{countries}},'_OTHER_') {
    $output.=" $country:".($bTimeByCountry{$country} / $delta);
  }
  $output=~s/^ //;
  if(open(REPORT,">$statsFiles{botCountry}")) {
    print REPORT $output;
    close(REPORT);
  }else{
    slog("Unable to write to stat file $statsFiles{botCountry}",1);
  }
  initBotCountryStats();
}

sub generateStateReport {
  my $output='';
  my $delta=time-$lastReportTimes{state};
  $delta=1 if($delta == 0);
  foreach my $state (@{$reportData{lobbyStates}}) {
    $output.=" $state:".($pTimeByState{$state} / $delta);
  }
  $output=~s/^ //;
  if(open(REPORT,">$statsFiles{state}")) {
    print REPORT $output;
    close(REPORT);
  }else{
    slog("Unable to write to stat file $statsFiles{state}",1);
  }
  initStateStats();
}

sub generateBotStateReport {
  my $output='';
  my $delta=time-$lastReportTimes{botState};
  $delta=1 if($delta == 0);
  foreach my $state (@{$reportData{lobbyStates}}) {
    $output.=" $state:".($bTimeByState{$state} / $delta);
  }
  $output=~s/^ //;
  if(open(REPORT,">$statsFiles{botState}")) {
    print REPORT $output;
    close(REPORT);
  }else{
    slog("Unable to write to stat file $statsFiles{botState}",1);
  }
  initBotStateStats();
}

sub generatePSizeReport {
  my $output='';
  my $delta=time-$lastReportTimes{pSize};
  $delta=1 if($delta == 0);
  foreach my $state (@{$reportData{battleStates}}) {
    foreach my $players (@{$reportData{nbPlayers}},'_OTHER_') {
      $output.=" ${state}_$players:".($pTimeByBState_Size{$state}->{$players} / $delta);
    }
  }
  $output=~s/^ //;
  if(open(REPORT,">$statsFiles{pSize}")) {
    print REPORT $output;
    close(REPORT);
  }else{
    slog("Unable to write to stat file $statsFiles{pSize}",1);
  }
  initPSizeStats();
}

sub generatePModReport {
  my $output='';
  my $delta=time-$lastReportTimes{pMod};
  $delta=1 if($delta == 0);
  foreach my $state (@{$reportData{battleStates}}) {
    foreach my $mod (keys %{$reportData{mods}},'_OTHER_') {
      $output.=" ${state}_$mod:".($pTimeByBState_Mod{$state}->{$mod} / $delta);
    }
  }
  $output=~s/^ //;
  if(open(REPORT,">$statsFiles{pMod}")) {
    print REPORT $output;
    close(REPORT);
  }else{
    slog("Unable to write to stat file $statsFiles{pMod}",1);
  }
  initPModStats();
}

sub generateBSizeReport {
  my $output='';
  my $delta=time-$lastReportTimes{bSize};
  $delta=1 if($delta == 0);
  foreach my $state (@{$reportData{battleStates}}) {
    foreach my $players (@{$reportData{nbPlayers}},'_OTHER_') {
      $output.=" ${state}_$players:".($bTimeByBState_Size{$state}->{$players} / $delta);
    }
  }
  $output=~s/^ //;
  if(open(REPORT,">$statsFiles{bSize}")) {
    print REPORT $output;
    close(REPORT);
  }else{
    slog("Unable to write to stat file $statsFiles{bSize}",1);
  }
  initBSizeStats();
}

sub generateBModReport {
  my $output='';
  my $delta=time-$lastReportTimes{bMod};
  $delta=1 if($delta == 0);
  foreach my $state (@{$reportData{battleStates}}) {
    foreach my $mod (keys %{$reportData{mods}},'_OTHER_') {
      $output.=" ${state}_$mod:".($bTimeByBState_Mod{$state}->{$mod} / $delta);
    }
  }
  $output=~s/^ //;
  if(open(REPORT,">$statsFiles{bMod}")) {
    print REPORT $output;
    close(REPORT);
  }else{
    slog("Unable to write to stat file $statsFiles{bMod}",1);
  }
  initBModStats();
}

sub generateNetworkReport {
  my $output='';
  my $delta=time-$lastReportTimes{network};
  $delta=1 if($delta == 0);

  my $avgPing="NaN";
  if($pingCount > 0) {
    $avgPing=$pingTime / $pingCount;
  }
  $output.="avgPing:$avgPing";

  foreach my $decoType (keys %gNbDecoByType) {
    $output.=" $decoType:".($gNbDecoByType{$decoType} / $delta);
  }
  if(open(REPORT,">$statsFiles{network}")) {
    print REPORT $output;
    close(REPORT);
  }else{
    slog("Unable to write to stat file $statsFiles{network}",1);
  }
  initNetworkStats();
}

# Bot commands handlers #####################################################

sub hHelp {
  my ($source,$user,$p_params)=@_;
  my ($cmd)=@{$p_params};

  if(defined $cmd) {
    my $helpCommand=lc($cmd);
    
    if(exists $botConf->{help}->{$helpCommand}) {

      my $p_help=$botConf->{help}->{$helpCommand};

      sayPrivate($user,"********** Help for command $cmd **********");
      sayPrivate($user,"Syntax:");
      sayPrivate($user,"  ".$p_help->[0]);
      sayPrivate($user,"Example(s):") if($#{$p_help} > 0);
      for my $i (1..$#{$p_help}) {
        sayPrivate($user,"  ".$p_help->[$i]);
      }

    }else{
      sayPrivate($user,"\"$cmd\" is not a valid command.");
    }
  }else{

    my $level=getUserAccessLevel($user);
    my $p_helpForUser=$botConf->getHelpForLevel($level);

    sayPrivate($user,"********** Available commands for your access level **********");
    foreach my $i (0..$#{$p_helpForUser->{direct}}) {
      sayPrivate($user,$p_helpForUser->{direct}->[$i]);
    }
  }

}

sub hHelpAll {
  my (undef,$user,undef)=@_;

  my $p_help=$botConf->{help};

  sayPrivate($user,"********** $conf{lobbyLogin} commands **********");
  for my $command (sort (keys %{$p_help})) {
    next unless($command);
    sayPrivate($user,$p_help->{$command}->[0]);
  }
}

sub hDumpStatus {
  my ($source,$user,$p_params)=@_;
  my %answerFunctions = ( pv => sub { sayPrivate($user,$_[0]) },
                          chan => sub { sayChan($conf{masterChannel},$_[0]) } );
  if($#{$p_params} != 0) {
    invalidSyntax($user,"dumpstatus");
    return 0;
  }

  if(exists $users{$p_params->[0]}) {
    my $p_status=$users{$p_params->[0]}->{status};
    foreach my $key (keys %{$p_status}) {
      answer("$key -> $p_status->{$key}");
    }
  }else{
    answer("$p_params->[0] is currently offine, unable to dump status");
  }
}

sub hMonitor {
  my ($source,$user,$p_params)=@_;
  my %answerFunctions = ( pv => sub { sayPrivate($user,$_[0]) },
                          chan => sub { sayChan($conf{masterChannel},$_[0]) } );
  $p_answerFunction=$answerFunctions{$source};
  if($#{$p_params} != 1) {
    invalidSyntax($user,"monitor");
    return 0;
  }

  if($p_params->[0] eq 'online') {
    if(exists $users{$p_params->[1]}) {
      answer("$p_params->[1] is currently online, unable to set notification");
    }else{
      $onlineMonitor{$p_params->[1]}={} unless(exists $onlineMonitor{$p_params->[1]});
      if(exists $onlineMonitor{$p_params->[1]}->{$user} && $onlineMonitor{$p_params->[1]}->{$user}) {
        answer("Notification already activated");
      }else{
        $onlineMonitor{$p_params->[1]}->{$user}=1;
        answer("Notification activated, you will be notified when $p_params->[1] is back online");
      }
    }
  }elsif($p_params->[0] eq 'endgame') {
    if(exists $users{$p_params->[1]}) {
      if($users{$p_params->[1]}->{status}->{inGame}) {
        $endgameMonitor{$p_params->[1]}={} unless(exists $endgameMonitor{$p_params->[1]});
        if(exists $endgameMonitor{$p_params->[1]}->{$user} && $endgameMonitor{$p_params->[1]}->{$user}) {
          answer("Notification already activated");
        }else{
          $endgameMonitor{$p_params->[1]}->{$user}=1;
          answer("Notification activated, you will be notified when $p_params->[1] is back from game");
        }
      }else{
        answer("$p_params->[1] is not in game, unable to set notification");
      }
    }else{
      answer("$p_params->[1] is not online, unable to set notification");
    }
  }elsif($p_params->[0] eq 'active') {
    if(exists $users{$p_params->[1]}) {
      if($users{$p_params->[1]}->{status}->{away}) {
        $activeMonitor{$p_params->[1]}={} unless(exists $activeMonitor{$p_params->[1]});
        if(exists $activeMonitor{$p_params->[1]}->{$user} && $activeMonitor{$p_params->[1]}->{$user}) {
          answer("Notification already activated");
        }else{
          $activeMonitor{$p_params->[1]}->{$user}=1;
          answer("Notification activated, you will be notified when $p_params->[1] is back from idle/away status");
        }
      }else{
        answer("$p_params->[1] is not idle/away, unable to set notification");
      }
    }else{
      answer("$p_params->[1] is not online, unable to set notification");
    }
  }elsif($p_params->[0] eq 'list' && getUserAccessLevel($user) >= 10) {
    if($p_params->[1] ne 'endgame' && $p_params->[1] ne 'active') {
      sayPrivate($user,"===== Online notifications =====");
      foreach my $monitoredUser (sort keys %onlineMonitor) {
        foreach my $notifiedUser (sort keys %{$onlineMonitor{$monitoredUser}}) {
          next unless($onlineMonitor{$monitoredUser}->{$notifiedUser});
          sayPrivate($user,"$monitoredUser is monitored by $notifiedUser");
        }
      }
    }
    if($p_params->[1] ne 'online' && $p_params->[1] ne 'active') {
      sayPrivate($user,"===== Endgame notifications =====");
      foreach my $monitoredUser (sort keys %endgameMonitor) {
        foreach my $notifiedUser (sort keys %{$endgameMonitor{$monitoredUser}}) {
          next unless($endgameMonitor{$monitoredUser}->{$notifiedUser});
          sayPrivate($user,"$monitoredUser is monitored by $notifiedUser");
        }
      }
    }
    if($p_params->[1] ne 'endgame' && $p_params->[1] ne 'online') {
      sayPrivate($user,"===== Active notifications =====");
      foreach my $monitoredUser (sort keys %activeMonitor) {
        foreach my $notifiedUser (sort keys %{$activeMonitor{$monitoredUser}}) {
          next unless($activeMonitor{$monitoredUser}->{$notifiedUser});
          sayPrivate($user,"$monitoredUser is monitored by $notifiedUser");
        }
      }
    }
  }else{
    invalidSyntax($user,"monitor");
    return 0;
  }

}

sub hQuit {
  my ($source,$user,undef)=@_;
  
  my %sourceNames = ( pv => "private",
                      chan => "channel #$conf{masterChannel}",
                      game => "game",
                      battle => "battle lobby" );

  scheduleQuit("requested by $user in $sourceNames{$source}");
}

sub hRestart {
  my ($source,$user,$p_params)=@_;
  my %sourceNames = ( pv => "private",
                      chan => "channel #$conf{masterChannel}");
  scheduleRestart("requested by $user in $sourceNames{$source}");
}

sub hVersion {
  my (undef,$user,undef)=@_;
  
  sayPrivate($user,"$conf{lobbyLogin} is running StatServ v$statServVer, with following components:");
  my %components = (SpringLobbyInterface => $lobby,
                    StatServConf => $botConf,
                    SimpleLog => $sLog);
  foreach my $module (keys %components) {
    my $ver=$components{$module}->getVersion();
    sayPrivate($user,"- $module v$ver");
  }

}

# Lobby interface callbacks ###################################################

sub cbPong {
  $timestamps{pong}=time;
  $pingCount++;
  $pingTime+=Time::HiRes::time-$timestamps{ping};
}

sub cbLobbyConnect {
  $lobbyState=2;
  my $requiredSpringVersion=$_[2];
  $timestamps{pong}=time;

  $lobby->addCallbacks({CHANNELTOPIC => \&cbChannelTopic,
                        PONG => \&cbPong,
                        LOGININFOEND => \&cbLoginInfoEnd,
                        JOIN => \&cbJoin,
                        JOINFAILED => \&cbJoinFailed,
                        SAID => \&cbSaid,
                        CHANNELMESSAGE => \&cbChannelMessage,
                        SAIDEX => \&cbSaidEx,
                        SAIDPRIVATE => \&cbSaidPrivate,
                        JOINED => \&cbJoined,
                        LEFT => \&cbLeft,
                        BROADCAST => \&cbBroadcast,
                        ADDUSER => \&cbAddUser,
                        REMOVEUSER => \&cbRemoveUser,
                        CLIENTSTATUS => \&cbClientStatus,
                        BATTLEOPENED => \&cbBattleOpened,
                        BATTLECLOSED => \&cbBattleClosed,
                        JOINEDBATTLE => \&cbJoinedBattle,
                        LEFTBATTLE => \&cbLeftBattle,
                        UPDATEBATTLEINFO => \&cbUpdateBattleInfo});

  my $localLanIp=$conf{localLanIp};
  $localLanIp=getLocalLanIp() unless($localLanIp);
  queueLobbyCommand(["LOGIN",$conf{lobbyLogin},$lobby->marshallPasswd($conf{lobbyPassword}),getCpuSpeed(),$localLanIp,"StatServ v$statServVer"],
                    {ACCEPTED => \&cbLoginAccepted,
                     DENIED => \&cbLoginDenied},
                    \&cbLoginTimeout);
}

sub cbBroadcast {
  my (undef,$msg)=@_;
  print "Lobby broadcast message: $msg\n";
  slog("Lobby broadcast message: $msg",3);
}

sub cbRedirect {
  my (undef,$ip)=@_;
  if($conf{lobbyFollowRedirect}) {
    slog("Following redirection to address $ip",3);
    computeAllUsers();
    computeAllBattles();
    %users=();
    %battles=();
    $lobbyState=0;
    foreach my $joinedChan (keys %{$lobby->{channels}}) {
      logMsg("channel_$joinedChan","=== $conf{lobbyLogin} left ===") if($conf{logChanJoinLeave});
    }
    $lobby->disconnect();
    $conf{lobbyHost}=$ip;
    $lobby = SpringLobbyInterface->new(serverHost => $conf{lobbyHost},
                                       serverPort => $conf{lobbyPort},
                                       simpleLog => $lobbySimpleLog,
                                       warnForUnhandledMessages => 0);
    $timestamps{connectAttempt}=0;
  }else{
    slog("Ignoring redirection request to address $ip",2);
  }
}

sub cbLobbyDisconnect {
  slog("Disconnected from lobby server (connection reset by peer)",2);
  computeAllUsers();
  computeAllBattles();
  %users=();
  %battles=();
  $lobbyState=0;
  foreach my $joinedChan (keys %{$lobby->{channels}}) {
    logMsg("channel_$joinedChan","=== $conf{lobbyLogin} left ===") if($conf{logChanJoinLeave});
  }
  $lobby->disconnect();
}

sub cbConnectTimeout {
  $lobbyState=0;
  slog("Timeout while connecting to lobby server ($conf{lobbyHost}:$conf{lobbyPort})",2);
}

sub cbLoginAccepted {
  $lobbyState=3;
  slog("Logged on lobby server",4);
}

sub cbLoginInfoEnd {
  $lobbyState=4;
  queueLobbyCommand(["JOIN",$conf{masterChannel}]) if($conf{masterChannel});
  if($conf{broadcastChannels}) {
    my @promChans=split(/;/,$conf{broadcastChannels});
    foreach my $chan (@promChans) {
      next if($chan eq $conf{masterChannel});
      queueLobbyCommand(["JOIN",$chan]);
    }
  }
}

sub cbLoginDenied {
  my (undef,$reason)=@_;
  slog("Login denied on lobby server ($reason)",1);
  $lobbyState=0;
  foreach my $joinedChan (keys %{$lobby->{channels}}) {
    logMsg("channel_$joinedChan","=== $conf{lobbyLogin} left ===") if($conf{logChanJoinLeave});
  }
  $lobby->disconnect();
}

sub cbLoginTimeout {
  slog("Unable to log on lobby server (timeout)",2);
  $lobbyState=0;
  foreach my $joinedChan (keys %{$lobby->{channels}}) {
    logMsg("channel_$joinedChan","=== $conf{lobbyLogin} left ===") if($conf{logChanJoinLeave});
  }
  $lobby->disconnect();
}

sub cbJoin {
  my (undef,$channel)=@_;
  slog("Channel $channel joined",4);
  logMsg("channel_$channel","=== $conf{lobbyLogin} joined ===") if($conf{logChanJoinLeave});
}

sub cbJoinFailed {
  my (undef,$channel,$reason)=@_;
  slog("Unable to join channel $channel ($reason)",2);
}

sub cbJoined {
  my (undef,$chan,$user)=@_;
  logMsg("channel_$chan","=== $user joined ===") if($conf{logChanJoinLeave});
}

sub cbLeft {
  my (undef,$chan,$user,$reason)=@_;
  my $reasonString='';
  if(defined $reason && $reason ne '') {
    $reasonString=" ($reason)" ;
    $quitReasons{$user}=$reason;
  }
  logMsg("channel_$chan","=== $user left$reasonString ===") if($conf{logChanJoinLeave});
}

sub cbSaid {
  my (undef,$chan,$user,$msg)=@_;
  logMsg("channel_$chan","<$user> $msg") if($conf{logChanChat});
  if($chan eq $conf{masterChannel} && $msg =~ /^!(\w.*)$/) {
    handleRequest("chan",$user,$1);
  }
}

sub cbChannelMessage {
  my (undef,$chan,$msg)=@_;
  logMsg("channel_$chan","* Channel message: $msg") if($conf{logChanChat});
}

sub cbSaidEx {
  my (undef,$chan,$user,$msg)=@_;
  logMsg("channel_$chan","* $user $msg") if($conf{logChanChat});
}

sub cbSaidPrivate {
  my (undef,$user,$msg)=@_;
  logMsg("pv_$user","<$user> $msg") if($conf{logPvChat});
  if($msg =~ /^!(\w.*)$/) {
    handleRequest("pv",$user,$1);
  }
}

sub cbChannelTopic {
  my (undef,$chan,$user,$time,$topic)=@_;
  logMsg("channel_$chan","* Topic is '$topic' (set by $user)") if($conf{logChanChat});
}


sub cbAddUser {
  my (undef,$user)=@_;
  $users{$user}=dclone($lobby->{users}->{$user});
  $cLastComputeTimes{$user}=time;
  if(exists $onlineMonitor{$user}) {
    foreach my $notifiedUser (keys %{$onlineMonitor{$user}}) {
      sayPrivate($notifiedUser,"$user is back online") if(exists $users{$notifiedUser});
    }
    delete $onlineMonitor{$user};
  }
}

sub cbRemoveUser {
  my (undef,$user)=@_;
  computeUser($user);
  if(defined $quitReasons{$user}) {
    my $reasonFound=0;
    foreach my $quitReason (keys %{$reportData{decoTypes}}) {
      if($quitReasons{$user} eq $quitReason) {
        $reasonFound=1;
        $gNbDecoByType{$reportData{decoTypes}->{$quitReason}}+=1;
        last;
      }
    }
    $gNbDecoByType{'_OTHER_'}+=1 unless($reasonFound);
  }else{
    $gNbDecoByType{unknown}+=1;
  }
  delete $users{$user};
}

sub cbClientStatus {
  my (undef,$user)=@_;

  computeUser($user);
  my $battleId;
  $battleId=$users{$user}->{battle} if(exists $users{$user});

  if(! exists $lobby->{users}->{$user}) {
    slog("Unable to handle CLIENTSTATUS command, user \"$user\" unknown in lobby module!",2);
    return;
  }

  if($users{$user}->{status}->{inGame} && ! $lobby->{users}->{$user}->{status}->{inGame}) {
    if(exists $endgameMonitor{$user}) {
      foreach my $notifiedUser (keys %{$endgameMonitor{$user}}) {
        sayPrivate($notifiedUser,"$user is back from game") if(exists $users{$notifiedUser});
      }
      delete $endgameMonitor{$user};
    }
  }

  if($users{$user}->{status}->{away} && ! $lobby->{users}->{$user}->{status}->{away}) {
    if(exists $activeMonitor{$user}) {
      foreach my $notifiedUser (keys %{$activeMonitor{$user}}) {
        sayPrivate($notifiedUser,"$user is back from idle/away status") if(exists $users{$notifiedUser});
      }
      delete $activeMonitor{$user};
    }
  }

  if(defined $battleId && $user eq $battles{$battleId}->{founder} && $users{$user}->{status}->{inGame} != $lobby->{users}->{$user}->{status}->{inGame}) {
    my $p_battleUsers=$battles{$battleId}->{userList};
    foreach my $bUser (@{$p_battleUsers}) {
      computeUser($bUser) unless($bUser eq $user);
    }
    if(! $users{$user}->{status}->{inGame}) {
      my $nbStartPlayers=$#{$p_battleUsers}+1-$battles{$battleId}->{nbSpec};
      if($nbStartPlayers < 1) {
        $nbStartPlayers=1;
      }elsif($nbStartPlayers > $battles{$battleId}->{maxPlayers}) {
        $nbStartPlayers=$battles{$battleId}->{maxPlayers};
      }
      $battles{$battleId}->{nbStartPlayers}=$nbStartPlayers;
    }
  }
  if(exists $lobby->{users}->{$user} && defined $lobby->{users}->{$user}) {
    $users{$user}=dclone($lobby->{users}->{$user});
    $users{$user}->{battle}=$battleId;
  }
}

sub cbBattleOpened {
  my (undef,$battleId,undef,undef,$user)=@_;
  computeUser($user);
  if(! exists $users{$user}) {
    slog("Unable to handle BATTLEOPENED message, user \"$user\" unknown!",1);
    return;
  }
  $users{$user}->{battle}=$battleId;
  $battles{$battleId}=dclone($lobby->{battles}->{$battleId});
  $bLastComputeTimes{$battleId}=time;
}

sub cbBattleClosed {
  my (undef,$battleId)=@_;
  my $p_battleUsers=$battles{$battleId}->{userList};
  foreach my $user (@{$p_battleUsers}) {
    computeUser($user);
    $users{$user}->{battle}=undef if(exists $users{$user});
  }
  computeBattle($battleId);
  delete $battles{$battleId};
}

sub cbUpdateBattleInfo {
  my (undef,$battleId)=@_;
  if(! exists $battles{$battleId}) {
    slog("Unable to handle UPDATEBATTLEINFO message, battle \"$battleId\" unknown!",1);
    return;
  }
  my $p_battleUsers=$battles{$battleId}->{userList};
  foreach my $user (@{$p_battleUsers}) {
    computeUser($user);
  }
  computeBattle($battleId);
  my $nbStartPlayers=$battles{$battleId}->{nbStartPlayers};
  $battles{$battleId}=dclone($lobby->{battles}->{$battleId});
  $battles{$battleId}->{nbStartPlayers}=$nbStartPlayers;
}

sub cbJoinedBattle {
  my (undef,$battleId,$user)=@_;
  computeUser($user);
  if(! exists $users{$user}) {
    slog("Unable to handle JOINEDBATTLE message, user \"$user\" unknown!",1);
    return;
  }
  $users{$user}->{battle}=$battleId;
  my $p_battleUsers=$battles{$battleId}->{userList};
  foreach my $bUser (@{$p_battleUsers}) {
    computeUser($bUser);
  }
  computeBattle($battleId);
  my $nbStartPlayers=$battles{$battleId}->{nbStartPlayers};
  $battles{$battleId}=dclone($lobby->{battles}->{$battleId});
  $battles{$battleId}->{nbStartPlayers}=$nbStartPlayers;
}

sub cbLeftBattle {
  my (undef,$battleId,$user)=@_;
  computeUser($user);
  if(! exists $users{$user}) {
    slog("Unable to handle LEFTBATTLE message, user \"$user\" unknown!",1);
    return;
  }
  $users{$user}->{battle}=undef;
  my $p_battleUsers=$battles{$battleId}->{userList};
  foreach my $bUser (@{$p_battleUsers}) {
    computeUser($bUser);
  }
  computeBattle($battleId);
  my $nbStartPlayers=$battles{$battleId}->{nbStartPlayers};
  $battles{$battleId}=dclone($lobby->{battles}->{$battleId});
  $battles{$battleId}->{nbStartPlayers}=$nbStartPlayers;
}

# Main ########################################################################

$sLog=$botConf->{log};

slog("Initializing StatServ",3);

initAllStats();

while($running) {

  if(! $lobbyState && ! $quitScheduled) {
    if($timestamps{connectAttempt} != 0 && $conf{lobbyReconnectDelay} == 0) {
      scheduleQuit("disconnected from lobby server, no reconnection delay configured");
    }else{
      if(time-$timestamps{connectAttempt} > $conf{lobbyReconnectDelay}) {
        $timestamps{connectAttempt}=time;
        $lobbyState=1;
        if(defined $lSock) {
          my @newSockets=();
          foreach my $sock (@sockets) {
            push(@newSockets,$sock) unless($sock == $lSock);
          }
          @sockets=@newSockets;
        }
        $lobby->addCallbacks({REDIRECT => \&cbRedirect});
        $lSock = $lobby->connect(\&cbLobbyDisconnect,{TASSERVER => \&cbLobbyConnect},\&cbConnectTimeout);
        $timestamps{ping}=time;
        $timestamps{pong}=time;
        if($lSock) {
          push(@sockets,$lSock);
        }else{
          $lobby->removeCallbacks(['REDIRECT']);
          $lobbyState=0;
          slog("Connection to lobby server failed",1);
        }
      }
    }
  }

  checkQueuedLobbyCommands();

  checkTimedEvents();

  my @pendingSockets=IO::Select->new(@sockets)->can_read(1);

  foreach my $pendingSock (@pendingSockets) {
    if($pendingSock == $lSock) {
      $lobby->receiveCommand();
    }else{
      scheduleQuit("Received data on unknown socket");
    }
  }

  if($lobbyState > 0 && (time - $timestamps{pong}) > 45) {
    slog("Disconnected from lobby server (ping timeout)",2);
    computeAllUsers();
    computeAllBattles();
    %users=();
    %battles=();
    $lobbyState=0;
    foreach my $joinedChan (keys %{$lobby->{channels}}) {
      logMsg("channel_$joinedChan","=== $conf{lobbyLogin} left ===") if($conf{logChanJoinLeave});
    }
    $lobby->disconnect();
  }

  if($lobbyState > 1 && (time - $timestamps{ping}) > 20) {
    sendLobbyCommand([["PING"]],5);
    $timestamps{ping}=Time::HiRes::time;
  }

  if($quitScheduled) {
    slog("No pending process, exiting",3);
    $running=0;
  }

}

if($lobbyState) {
  foreach my $joinedChan (keys %{$lobby->{channels}}) {
    logMsg("channel_$joinedChan","=== $conf{lobbyLogin} left ===") if($conf{logChanJoinLeave});
  }
  $lobby->disconnect();
}
if($quitScheduled == 2) {
  chdir($ENV{PWD});
  exec("$0 $confFile") || forkedError("Unable to restart $conf{lobbyLogin}",0);
}

exit 0;
