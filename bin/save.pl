#!/usr/bin/env perl

use strict;
use warnings;
use File::Copy;

my @cur_time=localtime();

my $year = $cur_time[5] += 1900;
my $month = $cur_time[4] += 1;
my $day = $cur_time[3];
my $hour = $cur_time[2];
my $min = $cur_time[1];

printf "year %d month %d day %d hour %d min %d\n", $year, $month, $day, $hour, $min;


foreach my $my_file (@ARGV) {
    my $to_file="~/Backup/".$my_file."_".$year."_".$month."_".$day."_".$hour."_".$min;
    copy($my_file, $to_file) or die "save file failed !!";
}

print "save file success !!\r\n";
