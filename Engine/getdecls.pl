#!/usr/bin/env perl
use strict;
use warnings;

sub count_func_params {
	my $p = shift;
	my @a = split /,/, $p;
	return scalar(@a);
}

sub get_return_size {
	my $t = shift;
	return 8 if($t =~ /\*/);
	return 0 if($t =~ /void/);
	return 4 if($t =~ /int/);
	return 1 if($t =~ /char/);
	return 1 if($t =~ /bool/);
	return 2 if($t =~ /short/);
	return 8 if($t =~ /longfloat/);
	return 8 if($t =~ /long/);
	return 4 if($t =~ /float/);
	return 999999999;
}

sub get_container_and_func {
	my $l = shift;
	my @a = split /:/, $l;
	return ($a[0], $a[1]);
}

sub func {
	my ($game, $real) = @_;
#	print "$game, $real\n";

my $cmd = << 'EOF';
find .. -name '*.[ch]*' | xargs grep 
EOF
	chomp($cmd);

	$cmd .= $real;
#	print("$cmd\n");

	my @findres = `$cmd`;
	my $found = 0;

	for(@findres) {
		if(/([\w_]+\s*\*{0,1})\s*\Q$real\E\s*\((.*)?\)\s*{/) {
			my $save = $_;
			my ($container, $decl) = get_container_and_func($_);
			chomp($decl); chomp($container);
			my $newdecl = $decl;
			my $returntype = $1;
			my $params = $2;
			my $numparams = count_func_params($params);
			my $returnsize = get_return_size($returntype);
			if($returnsize == 0) {
				$decl =~ s/void/long/;
			} elsif($returnsize == 4) {
				if($returntype =~ /int/) {
					$decl =~ s/int/long/;
				} elsif($returntype =~ /float/) {
					$decl =~ s/float/longfloat/;
				} else {
					die("oopsie");
				}
			} elsif($returnsize == 1) {
				$decl =~ s/char/long/;
			} elsif ($returnsize == 8) {
				die "oopsie5" unless $returntype =~ /\*/ || $returntype =~ /longfloat/ || $returntype =~ /long/;
			} elsif ($returntype =~ /CharTo/ || $returntype =~ /Character_/ || $returntype =~ /Game_/  || $returntype eq "__" || $returntype eq "DateTime_"
				|| $returntype eq "Is" || $returntype eq "Un" || $returntype eq "AboutTo" || $returntype eq "Parser_"
				|| $returntype eq "Object_" || $returntype eq "_sc_" || $returntype eq "u" || $returntype eq "check_") {
				# false positive for Display:  int GUIInv::CharToDisplay() {
				next;
			} else {
				
				print "$real:$save:$container:$decl xxx $returnsize $returntype\n";
				die("oopsie1" . $returntype);
			}
			print $_;
# void createFunctionImport(char* gamefunc, void* fp, size_t returnsize, size_t numargs, ... /* size of each arg */);
			printf 'createFunctionImport("%s", (void*) %s, %d, %d', $game, $real, $returnsize, $numparams; 
			print");\n";
			my @args = split /,/, $params;
			for(@args) {
				next if /\.\.\./;
				next if $_ eq "void";
				if(/(const\s+){0,1}([\w_]+\s*\*{0,1})\s*([\w_]+)/) {
					my $t = $2;
					my $n = $3;
					$t =~ s/\s+//g;
					$n =~ s/\s+//g;
					next if $t =~ /\*/;
					if($t =~ /int/ || $t =~ /char/ || $t =~ /bool/) {
						my $nu = "long $n";
						$decl =~ s/\Q$_\E/$nu/;
					} elsif($t =~ /longfloat/ || $t =~ /long/) {
					} elsif($t =~ /float/) {
                                                my $nu = "longfloat $n";
                                                $decl =~ s/\Q$_\E/$nu/;
					} else { die("oopsie4" . $t); }
				} else { die("oopise3"); }
			}

			my $sedcmd = sprintf 'sed -i s\'@%s@%s@\' %s'. "\n", $newdecl, $decl, $container;
			system($sedcmd);
			$found = 1;
			last;
		}
	}
	warn "$real\n" unless $found;
}

while(<>) {
	my $decl = 0;
	$decl = 1 if(/scAdd_External_Symbol/);

	if(/scAdd_External_Symbol\(\"([\w:^_]+)\",\s*\(void\s*\*\)([\w_]+)\)/) {
		my $gamefunc = $1;
		my $realfunc = $2;
		#print "$1, $2\n";
		func($gamefunc, $realfunc);

	} elsif($decl) {
		print "failed to detect $_\n";
	}

}
