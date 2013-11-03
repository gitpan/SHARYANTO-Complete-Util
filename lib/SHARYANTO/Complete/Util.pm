package SHARYANTO::Complete::Util;

use 5.010001;
use strict;
use warnings;
#use Log::Any '$log';

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(
                       complete_array
                       complete_hash_key
                       complete_env
                       complete_file
                       complete_program

                       parse_shell_cmdline
               );

our $VERSION = '0.02'; # VERSION

our %SPEC;

$SPEC{complete_array} = {
    v => 1.1,
    args => {
        array => { schema=>['array*'=>{of=>'str*'}], pos=>0, req=>1 },
        word  => { schema=>[str=>{default=>''}], pos=>1 },
        ci    => { schema=>[bool=>{default=>0}] },
    },
    result_naked => 1,
};
sub complete_array {
    my %args  = @_;
    #$log->tracef("=> complete_array(%s)", \%args);
    my $array = $args{array} or die "Please specify array";
    my $word  = $args{word} // "";
    my $ci    = $args{ci};

    my $wordu = uc($word);
    my @words;
    for (@$array) {
        next unless 0==($ci ? index(uc($_), $wordu) : index($_, $word));
        push @words, $_;
    }
    $ci ? [sort {lc($a) cmp lc($b)} @words] : [sort @words];
}

$SPEC{complete_hash_key} = {
    v => 1.1,
    args => {
        hash  => { schema=>['hash*'=>{}], pos=>0, req=>1 },
        word  => { schema=>[str=>{default=>''}], pos=>1 },
        ci    => { schema=>[bool=>{default=>0}] },
    },
    result_naked => 1,
};
sub complete_hash_key {
    my %args  = @_;
    #$log->tracef("=> complete_hash_key(%s)", \%args);
    my $hash  = $args{hash} or die "Please specify hash";
    my $word  = $args{word} // "";
    my $ci    = $args{ci};

    complete_array(word=>$word, array=>[keys %$hash], ci=>$ci);
}

$SPEC{complete_env} = {
    v => 1.1,
    args => {
        word  => { schema=>[str=>{default=>''}], pos=>0 },
        ci    => { schema=>[bool=>{default=>0}] },
    },
    result_naked => 1,
};
sub complete_env {
    my %args  = @_;
    #$log->tracef("=> complete_env(%s)", \%args);
    my $word  = $args{word} // "";
    my $ci    = $args{ci};
    if ($word =~ /^\$/) {
        complete_array(word=>$word, array=>[map {"\$$_"} keys %ENV], ci=>$ci);
    } else {
        complete_array(word=>$word, array=>[keys %ENV], ci=>$ci);
    }
}

$SPEC{complete_program} = {
    v => 1.1,
    args => {
        word  => { schema=>[str=>{default=>''}], pos=>0 },
    },
    result_naked => 1,
};
sub complete_program {
    require List::MoreUtils;

    my %args  = @_;
    #$log->tracef("=> complete_program(%s)", \%args);
    my $word  = $args{word} // "";

    my @words;
    my @dir;
    my $word_has_path;
    $word =~ m!(.*)/(.*)! and do { @dir = ($1); $word_has_path++; $word = $2 };
    @dir = split /:/, $ENV{PATH} unless @dir;
    unshift @dir, ".";
    for my $dir (@dir) {
        $dir =~ s!/+$!!; #TEST
        opendir my($dh), $dir or next;
        for (readdir($dh)) {
            next if $word !~ /^\.\.?$/ && ($_ eq '.' || $_ eq '..');
            next unless index($_, $word) == 0;
            next unless (-x "$dir/$_") && (-f _) ||
                ($dir eq '.' || $word_has_path) && (-d _);
            push @words, (-d _) ? "$_/" : $_;
        };
    }

    complete_array(array=>[List::MoreUtils::uniq(@words)]);
}

$SPEC{complete_file} = {
    v => 1.1,
    args => {
        word => { schema=>[str=>{default=>''}], pos=>0 },
        f    => { summary => 'Whether to include file',
                  schema=>[bool=>{default=>1}] },
        d    => { summary => 'Whether to include directory',
                  schema=>[bool=>{default=>1}] },
    },
    result_naked => 1,
};
sub complete_file {
    my %args  = @_;
    #$log->tracef("=> complete_file(%s)", \%args);
    my $word  = $args{word} // "";
    my $f     = $args{f} // 1;
    my $d     = $args{d} // 1;

    my @all;
    if ($word =~ m!(\A|/)\z!) {
        my $dir = length($word) ? $word : ".";
        opendir my($dh), $dir or return [];
        @all = map { ($dir eq '.' ? '' : $dir) . $_ }
            grep { $_ ne '.' && $_ ne '..' } readdir($dh);
        closedir $dh;
    } else {
        # must add wildcard char, glob() is convoluted. also {a,b} is
        # interpreted by glob() (not so by bash file completion). also
        # whitespace is interpreted by glob :(. later should replace with a
        # saner one, like wildcard2re.
        @all = glob("$word*");
    }

    my @words;
    for (@all) {
        next if (-f $_) && !$f;
        next if (-d _ ) && !$d;
        $_ = "$_/" if (-d _) && !m!/\z!;
        #s!.+/(.+)!$1!;
        push @words, $_;
    }

    my $w = complete_array(array=>\@words);

    # this is a trick so that when completion is a single dir/, bash does not
    # insert a space but still puts the cursor after "/", just like when it's
    # doing dir completion.
    if (@$w == 1 && $w->[0] =~ m!/\z!) { $w->[1] = "$w->[0] ";  }

    $w;
}

# current problems: Can't parse unclosed quotes (e.g. spanel get-plan "BISNIS
# A<tab>) and probably other problems, since we don't have access to COMP_WORDS
# like in shell functions.
sub _line_to_argv {
    require IPC::Open2;

    my $line = pop;
    my $cmd = q{_pbc() { for a in "$@"; do echo "$a"; done }; _pbc } . $line;
    my ($reader, $writer);
    my $pid = IPC::Open2::open2($reader,$writer,'bash 2>/dev/null');
    print $writer $cmd;
    close $writer;
    my @array = map {chomp;$_} <$reader>;

    # We don't want to expand ~ for user experience and to be consistent with
    # Bash's behavior for tab completion (as opposed to expansion of ARGV).
    my $home_dir = (getpwuid($<))[7];
    @array = map { s!\A\Q$home_dir\E(/|\z)!\~$1!; $_ } @array;

    \@array;
}

# simplistic parsing, doesn't consider shell syntax at all. doesn't work the
# minute we use funny characters.
#sub _line_to_argv_BC {
#    [split(/\h+/, $_[0])];
#}

$SPEC{parse_shell_cmdline} = {
    v => 1.1,
    summary => 'Parse shell command-line for processing by completion routines',
    description => <<'_',

Currently only supports bash.

Returns hash with the following keys: `words` (array of str, equivalent to
`COMP_WORDS` provided by shell to completion routine), `cword` (int, equivalent
to shell-provided `COMP_CWORD`).

_
    args_as => 'array',
    args => {
        cmdline => {
            summary => 'Command-line, defaults to COMP_LINE environment',
            schema => 'str*',
            pos => 0,
        },
        point => {
            summary => 'Point/position to complete in command-line, '.
                'defaults to COMP_POINT',
            schema => 'int*',
            pos => 1,
        },
        opts => {
            schema => 'hash*',
            description => <<'_',

Currently known options: parse_line_sub (code).

_
            pos => 2,
        },
    },
    result_naked => 1,
};
sub parse_shell_cmdline {
    my ($line, $point, $opts) = @_;
    #$log->tracef("-> parse_shell_cmdline(%s, %s)", $line, $point);
    $opts //= {};
    $opts->{parse_line_sub} //= \&_line_to_argv;

    $line  //= $ENV{COMP_LINE};
    $point //= $ENV{COMP_POINT};
    #$log->tracef("line=q(%s), point=%s", $line, $point);

    my $left  = substr($line, 0, $point);
    my @left;
    if (length($left)) {
        @left = @{ $opts->{parse_line_sub}->($left) };
        # shave off $0
        substr($left, 0, length($left[0])) = "";
        $left =~ s/^\s+//;
        shift @left;
    }

    my $right = substr($line, $point);
    my @right;
    if (length($right)) {
        $right =~ s/^\S+//;
        @right = @{ $opts->{parse_line_sub}->($right) } if length($right);
    }
    #$log->tracef("left=q(%s), \@left=%s, right=q(%s), \@right=%s",
    #             $left, \@left, $right, \@right);

    my $words = [@left, @right],
    my $cword = @left ? scalar(@left)-1 : 0;

    # is there a space after the final word (e.g. "foo bar ^" instead of "foo
    # bar^" or "foo bar\ ^")? if yes then cword is on the next word.
    my $tmp = $left;
    my $nspc_left = 0; $nspc_left++ while $tmp =~ s/\s$//;
    $tmp = $left[-1];
    my $nspc_lastw = 0;
    if (defined($tmp)) { $nspc_lastw++ while $tmp =~ s/\s$// }
    $cword++ if $nspc_lastw < $nspc_left;

    my $res = {words => $words, cword => $cword};
    #$log->tracef("<- _parse_request, result=%s", $res);
    $res;
}

1;
# ABSTRACT: Shell tab completion routines

__END__

=pod

=encoding utf-8

=head1 NAME

SHARYANTO::Complete::Util - Shell tab completion routines

=head1 VERSION

version 0.02

=head1 FUNCTIONS


=head2 complete_array(%args) -> [status, msg, result, meta]

Arguments ('*' denotes required arguments):

=over 4

=item * B<array>* => I<array>

=item * B<ci> => I<bool> (default: 0)

=item * B<word> => I<str> (default: "")

=back

Return value:

Returns an enveloped result (an array). First element (status) is an integer containing HTTP status code (200 means OK, 4xx caller error, 5xx function error). Second element (msg) is a string containing error message, or 'OK' if status is 200. Third element (result) is optional, the actual result. Fourth element (meta) is called result metadata and is optional, a hash that contains extra information.

=head2 complete_env(%args) -> [status, msg, result, meta]

Arguments ('*' denotes required arguments):

=over 4

=item * B<ci> => I<bool> (default: 0)

=item * B<word> => I<str> (default: "")

=back

Return value:

Returns an enveloped result (an array). First element (status) is an integer containing HTTP status code (200 means OK, 4xx caller error, 5xx function error). Second element (msg) is a string containing error message, or 'OK' if status is 200. Third element (result) is optional, the actual result. Fourth element (meta) is called result metadata and is optional, a hash that contains extra information.

=head2 complete_file(%args) -> [status, msg, result, meta]

Arguments ('*' denotes required arguments):

=over 4

=item * B<d> => I<bool> (default: 1)

Whether to include directory.

=item * B<f> => I<bool> (default: 1)

Whether to include file.

=item * B<word> => I<str> (default: "")

=back

Return value:

Returns an enveloped result (an array). First element (status) is an integer containing HTTP status code (200 means OK, 4xx caller error, 5xx function error). Second element (msg) is a string containing error message, or 'OK' if status is 200. Third element (result) is optional, the actual result. Fourth element (meta) is called result metadata and is optional, a hash that contains extra information.

=head2 complete_hash_key(%args) -> [status, msg, result, meta]

Arguments ('*' denotes required arguments):

=over 4

=item * B<ci> => I<bool> (default: 0)

=item * B<hash>* => I<hash>

=item * B<word> => I<str> (default: "")

=back

Return value:

Returns an enveloped result (an array). First element (status) is an integer containing HTTP status code (200 means OK, 4xx caller error, 5xx function error). Second element (msg) is a string containing error message, or 'OK' if status is 200. Third element (result) is optional, the actual result. Fourth element (meta) is called result metadata and is optional, a hash that contains extra information.

=head2 complete_program(%args) -> [status, msg, result, meta]

Arguments ('*' denotes required arguments):

=over 4

=item * B<word> => I<str> (default: "")

=back

Return value:

Returns an enveloped result (an array). First element (status) is an integer containing HTTP status code (200 means OK, 4xx caller error, 5xx function error). Second element (msg) is a string containing error message, or 'OK' if status is 200. Third element (result) is optional, the actual result. Fourth element (meta) is called result metadata and is optional, a hash that contains extra information.

=head2 parse_shell_cmdline(%args) -> [status, msg, result, meta]

Parse shell command-line for processing by completion routines.

Currently only supports bash.

Returns hash with the following keys: C<words> (array of str, equivalent to
C<COMP_WORDS> provided by shell to completion routine), C<cword> (int, equivalent
to shell-provided C<COMP_CWORD>).

Arguments ('*' denotes required arguments):

=over 4

=item * B<cmdline> => I<str>

Command-line, defaults to COMP_LINE environment.

=item * B<opts> => I<hash>

Currently known options: parseI<line>sub (code).

=item * B<point> => I<int>

Point/position to complete in command-line, defaults to COMP_POINT.

=back

Return value:

Returns an enveloped result (an array). First element (status) is an integer containing HTTP status code (200 means OK, 4xx caller error, 5xx function error). Second element (msg) is a string containing error message, or 'OK' if status is 200. Third element (result) is optional, the actual result. Fourth element (meta) is called result metadata and is optional, a hash that contains extra information.

=head1 HOMEPAGE

Please visit the project's homepage at L<https://metacpan.org/release/SHARYANTO-Complete-Util>.

=head1 SOURCE

Source repository is at L<https://github.com/sharyanto/perl-SHARYANTO-Complete-Util>.

=head1 BUGS

Please report any bugs or feature requests on the bugtracker website
http://rt.cpan.org/Public/Dist/Display.html?Name=SHARYANTO-Complete-Util

When submitting a bug or request, please include a test-file or a
patch to an existing test-file that illustrates the bug or desired
feature.

=head1 AUTHOR

Steven Haryanto <stevenharyanto@gmail.com>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2013 by Steven Haryanto.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut
