#!/usr/bin/env perl
# https://adventofcode.com/2018/day/5 in Perl 5
# Try it on https://tio.run/#perl (put input in the Input field)

use List::Util qw(min);
my $re = join '|', map {$_.uc, uc.$_} a..z;
my $in = $_ = <>;
1 while s/$re//g;
print length, "\n";
print min map {my $v = $in; 1 while $v =~ s/$re|$_|\U$_//g; length $v} a..z;
