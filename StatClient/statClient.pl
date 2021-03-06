#!/usr/bin/perl -w
#
# SpringRTS lobby statistics client. This application is used to poll statistics
# generated by StatServ. The statistic numbers are accurate averages since last
# poll request (StatServ automatically adjust the statistic period acccording to
# polling interval).
#
# Following statistics can be requested:
# - rank (player statistics by rank)
# - botRank (bot statistics by rank)
# - country (player statistics by country)
# - botCountry (bot statistics by country)
# - state (player statistics by status)
# - botState (bot statistics by status)
# - pSize (player statistics by battle size)
# - pMod (player statistics by mod)
# - bSize (battle statistics by size)
# - bMod (battle statistics by mod)
# - network (network statistics)
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

my $statDir='<statDir>';
my @stats=qw/rank botRank country botCountry state botState pSize pMod bSize bMod network/;

if($#ARGV != 0 || ! grep {$ARGV[0] eq $_} @stats) {
  my $statsString=join('|',@stats);
  print "usage: $0 $statsString\n";
  exit 1;
}

my $statFile="$statDir/$ARGV[0].stats";
if(-f $statFile) {
  if(open(STAT,"<$statFile")) {
    while(<STAT>) {
      print $_;
    }
    close(STAT);
    unlink($statFile);
  }else{
    print "ERROR - Unable to open file $statFile\n";
  }
}else{
  print "ERROR - Unable to find file $statFile\n";
}
