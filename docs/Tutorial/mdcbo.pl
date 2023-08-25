#!/usr/bin/env perl

use strict;
use warnings;

use File::Temp qw/tempfile tempdir/;
use File::Spec;
use Getopt::Long;
use Pod::Usage;

#
# Globals
# 	- $file_md: markdown file given as input
# 	- $file_md_sh: the extracted bash commands from $file_md
# 	- $out: the output file
#
my $file_md = undef;
my $file_md_sh;
my $out = undef;

my $help = 0;
my $man = 0;

# Regex for bash code blocks
# Loosely follows:
#  	- https://spec.commonmark.org/0.30/#preliminaries
#  	- https//spec.commonmark.org/0.30/#fenced-code-blocks
# Warning: this doesn't exactly follow the spec... It seems unlikely that we
# can do this with a "simple" regex.
my $line_ending_regex = qr/\x{000A}|(?:\x{000D}[^\x{000A}])|(?:\x{000D}\x{000A})/;
my $line_regex = qr/[^\x{000A}\x{000D}]*${line_ending_regex}/;
my $space_regex = qr/\x{0020}/;
my $tab_regex = qr/\x{0009}/;
my $space_or_tab_regex = qr/${space_regex}|${tab_regex}/;
my $bash_backtick_fenced_code_block_regex = qr/
	# Identation (note: this ignores the context indentation)
	(?<indent>${space_regex}*)
		# Opening code fence
		(?<open>`{3,})
			# Info string
			(?:${space_or_tab_regex})*bash(?:${space_or_tab_regex})*${line_ending_regex}
			# Contents
			(?<contents>\g{indent}(?:${line_regex})*?)
		# Closing code fence
		\g{indent}\g{open}`*${space_or_tab_regex}*
		/x;
my $bash_tilde_fenced_code_block_regex = qr/
	# Warning: duplicated code from $bash_backtick_fenced_code_block_regex
	# Identation (note: this ignores the context indentation)
	(?<indent>$space_regex*)
		# Opening code fence
		(?<open>~{3,})
			# Info string
			(?:${space_or_tab_regex})*bash(?:${space_or_tab_regex})*${line_ending_regex}
			# Contents
			(?<contents>\g{indent}(?:${line_regex})*?)
		# Closing code fence
		\g{indent}\g{open}~*${space_or_tab_regex}*
		/x;
my $bash_code_block_regex = qr/
	(?:${line_ending_regex}|^)
		(?:${bash_backtick_fenced_code_block_regex}|${bash_tilde_fenced_code_block_regex})
	(?:${line_ending_regex}|$)
	/sx;

#
# Get CLI options
#
GetOptions(
	"output|o=s" => \$out,
	"help|?" => \$help,
	"man" => \$man
	) or pod2usage(2);

pod2usage(1) if $help;
pod2usage(-exitval => 0, -verbose => 2) if $man;

pod2usage("$0: Expected at most one input file") if (!(scalar(@ARGV) <= 1));

#
# Open file handles / set globals
#

my $out_handle;
if (defined($out)) {
	open($out_handle, ">", $out)
		or die "Can't open > $out: $!";
}  else {
	$out_handle = "STDOUT";
}

my $file_md_handle;
if (defined($file_md = $ARGV[0])) {
	open($file_md_handle, "<", $file_md)
		or die "Can't open < $file_md: $!";
} else {
	$file_md_handle = "STDIN";
}

my $file_md_sh_handle;
if (defined($file_md)) {
	$file_md_sh = "$file_md.mdcbo.sh";
	open($file_md_sh_handle, ">", $file_md_sh)
		or die "Can't open > ${file_md_sh}: $!";
} else {
	($file_md_sh_handle, $file_md_sh) = tempfile();
}

#
# Main algorithm
#

# Overview.
# 	1. Load the argument $file_md in memory
# 	2. Extract the '```bash $cmd ```' fenced code blocks from $file_md into
# 	a a single aggregated file $file_md_sh.
#
#	More precisely, we will start with putting
#	'
#	set -xe
#	'
#	in $file_md_sh so that users can see what is being executed while it is
#	running (makes debugging easier!) + we want the program to fail if
#	_anything_ fails.
#
#	Then, for every bash code block of the form in $file_md
# 	'
#	```bash
#	$cmd
#	```
#	'
#
# 	we will append the following command in the $file_md_sh
#
# 	'
#	echo "$tmp_dir/$tmp_file"
#	{
#	$cmd
#	} > "$tmp_dir/$tmp_file"
#	'
#
# 	3. Execute $file_md_sh, and copy $file_md to $out but whenever
#	we see a bash code block like
#	'
#	```bash
#	$cmd
#	```
#	'
#	copy the follwing
#	'
#	Standard output.
#	```bash
#	<stdout of $cmd>
#	```
#	'
#	to $out.

# 1.
my $file_md_contents;

{
	local $/ = undef;
	$file_md_contents = <$file_md_handle>;
}

# 2.
my $tmp_dir = tempdir("mdcbo.pl.XXXXXX", CLEANUP => 1, Dir => File::Spec->tmpdir);

{
	local $\ = "\n";
	select $file_md_sh_handle;

	print "set -xe";

	while ($file_md_contents =~ /$bash_code_block_regex/g) {
		print "MDCBO_PL_TMP=\$(mktemp -p $tmp_dir)";
		print "echo \"\$MDCBO_PL_TMP\"";
		print "{";
		print $+{"contents"};
		print "} > \"\$MDCBO_PL_TMP\"";
	}
}

# 3.
{

	my $file_md_sh_abs = File::Spec->rel2abs($file_md_sh); # nix is weird, so we get the absolute path
	my $file_md_sh_cmd = ". $file_md_sh_abs";
	my $file_md_sh_output_contents = `$file_md_sh_cmd`;
	die "Script failed to execute: $!"
		if $? == -1;
	die "Child died with signal", ($? & 127), ",",  ($? & 128) ? 'with' : 'without', "coredump"
		if $? & 127;
	die "Script exited with error: ", ($? >> 8)
		if ($? >> 8) != 0;
	my @file_md_sh_output_contents =
		split(/\n/, $file_md_sh_output_contents);

	# Invariants:
	# 	- i,j are the indices in the $file_md_contents which delimits
	# 	bash code blocks
	my $i = 0;
	my $j = 0;
	my $cmd_output;

	pos($file_md_contents) = $j = $i = 0;

	select $out_handle;
	local $\ = undef;
	while ( ($file_md_contents =~ /$bash_code_block_regex/g),
		($cmd_output = shift (@file_md_sh_output_contents)),
			(defined(pos($file_md_contents))
				&& defined($cmd_output))) {
		$j = pos($file_md_contents);
		my $indent = length($+{"indent"});

		print substr($file_md_contents, $i, $j - $i);

		{
			open(my $cmd_output_handle, "<", $cmd_output)
				or die "Can't open a command's output: $!";

			local $/ = undef;
			my $cmd_output_contents = <$cmd_output_handle>;

			if ($cmd_output_contents ne "") {
				local $\ = "\n";

				print "";
				print " " x $indent, "Standard output.";
				print " " x $indent, "```";

				foreach (split(/\n/, $cmd_output_contents)) {
					print " " x $indent, $_;
				}

				print " " x $indent,  "```";
			}
			close($cmd_output_handle);
		}

		$i = $j;
	}

	# Prints the remaining part of the file (everything after the last
	# regex match
	print substr($file_md_contents, $i);

	die "Internal error: mismatched bash code blocks and command outputs."
		if (!(!defined(pos($file_md_contents)) && !defined($cmd_output)));
}

__END__
=head1 mdcbo.pl

mdcbo.pl - Augmenting a markdown file's fenced code blocks with info string as
bash with their corresponding standard output as if they were run in sequence.

=head1 SYNOPSIS

mdcbo.pl [options] file

=head1 OPTIONS

=over 8

=item B<--help>

Print a brief help message and exits.

=item B<--man>

Prints the manual page and exits.

=item B<-o,--output>

Output file

=back

=head1 DESCRIPTION

B<mdcbo.pl> will read the given markdown input file (defaults to stdin) and
locates all fenced code blocks with info string bash, runs the commands in the
fenced code blocks in sequence (in a single shell script), and outputs a file
(defaults to stdout) identical to the input file with the stdout of the
individual command pasted after each fenced code block.

=head1 BUGS

The parsing of fenced code blocks uses a regex which is not fully standards
compliant with Common Markdown Spec https://spec.commonmark.org/0.30/.
In particular, it incorrectly parses container blocks (e.g. block quotes and
lists) which contain fenced code blocks.
This is because B<mdcbo.pl> identifies fenced code blocks by simply looking for
a possibly indented the open code fence C<```bash>, and verifying that the
contents of the fenced code block and the close code fence  both have at least
the same indentation.

So, with block quotes, one may have a markdown document
 # Title
 > ```bash
 > echo pomeranian
 > ```
which clearly contains an open code block, but B<mdcbo.pl> will not identify
this as a fenced code block as the fenced code block is not preceeded by
exclusively space indentation.

And with lists, one may have the markdown document
 # Title
  ```bash
 echo pomeranian
   ```
which clearly contains a fenced code block, but B<mdcbo.pl> would not recognize
this as a fenced code block as it violates B<mdcbo.pl>'s indentation rule.

Similarly with indented code blocks, the markdown document
  # Title
       ```bash
       Not a fenced code block!
       ```
contains an indented code block instead of a fenced code block which
B<mdcbo.pl> would incorrectly regard as a fenced code block.

These may actually be regarded as a feature to B<not> execute the bash code.

To get around this, it would be a good idea to use something else that has a
real markdown parser. As an improvement, one could then use a different info
string to filter out the fenced code blocks one would like to execute.

=cut
