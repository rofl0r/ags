#!/usr/bin/env perl
use strict;
use warnings;

use File::Slurp;

sub get_container_and_func {
	my $l = shift;
	my @a = split /:/, $l;
	return ($a[0], $a[1]);
}
while(<>) {
	chomp;
	my $fnc = $_;
	my $cmd = << 'EOF';
find .. -name '*.[ch]*' | xargs grep 
EOF
	chomp($cmd);

	$cmd .= $_;

	my @findres = `$cmd`;
	my $found = 0;

	for(@findres) {
		$found = 0;
		if(/([\w_]+\s*\*{0,1})\s*\Q$fnc\E\s*\((.*)?\)\s*$/) {
			my ($container, $decl) = get_container_and_func($_);
			my @c = read_file($container);
			my $f = 0;
			my $l = 0;
			for(@c) {
				$l++;
				if($f) {
					if(/^\s*{/) {
						print("$container : $l   : $fnc \n");
						$found = 1;
						last;
					}
					$f = 0;
				}
				$f = 1 if(/\Q$decl\E/);
	

			}
		}
		last if $found;
	}

}
