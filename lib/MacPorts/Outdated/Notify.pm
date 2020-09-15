## no critic (Documentation::RequirePodAtEnd Documentation::RequirePodSections InputOutput::RequireCheckedSyscalls CodeLayout::RequireTidyCode)
package MacPorts::Outdated::Notify;

use 5.010;
use strict;
use warnings;

use Data::Dumper;
use Pod::Usage;
use Getopt::Long;
use File::Slurp;
use File::Util qw{SL};
use JSON::MaybeXS;
use English qw{-no_match_vars};
use Cwd;
use Carp;
use YAML;
use Capture::Tiny;
use Term::ANSIColor;
use DateTime;
use Readonly;
use Clone qw(clone);

Readonly our $VERSION        => q{0.0.1};
Readonly my $DEFAULT_CONFIG  => {
    valid                       => 0,
    use_cache                   => 1,
    no_rc_file                  => 0,
    verbose                     => 0,
    quiet                       => 0,
    config_file                 => $ENV{HOME} . SL . q{.portcheck.yml},
    cache_file                  => $ENV{HOME} . SL . q{.portcheck.cache}, #
    cache_expire_days           => 7,
    warn_about_outdated_defs    => 1,
    debug                       => 0,
    port_executable             => q{/opt/local/bin/port},
    exit_codes                  => {
        all_good                    => 0,
        general_error               => 1,
        only_ports_outdated         => 200,
        only_defs_outdated          => 201,
        both_outdated               => 202,
    },
};

sub new {
    my $class = shift;
    my $self = {
        CONFIG      => { %{$DEFAULT_CONFIG} },
        CACHE       => {
            valid   => 0
        },
        OUTDATED    => undef,
        DEBUG_MSGS  => [],
    };

    bless $self, $class;
    return $self;
}

sub get_config {
    my $self = shift;
    my $key  = shift;

    if($key && !defined($self->{CONFIG}->{$key})) {
        $self->error(q{Parameter given, but does not exist});
        return;
    }

    return ($key) ? $self->{CONFIG}->{$key} : $self->{CONFIG};
}

sub set_config {
    my $self  = shift;
    my $key   = shift // return;
    my $value = shift // return;

    $self->{CONFIG}->{$key} = $value;
    return $value;
}

sub get_cache {
    my $self = shift;
    return $self->{CACHE};
}

sub get_outdated {
    my $self = shift;
    return $self->{OUTDATED};
}

## no critic (ProhibitNoStrict Subroutines::RequireArgUnpacking)
sub error {
    my $self = shift;
    my $errvar;

    {
        no strict qw( refs );
        $errvar = ref $self ? \$self->{ ERROR } : \${"$self\::ERROR"};
    }
    if (@_) {
        ${$errvar} = ref($_[0]) ? shift : join q{}, @_;
        return;
    }
    else {
        return ${$errvar};
    }
}

sub to_string {
    my $self = shift;
    return 'foo';
}

sub run {
    my ($self, $args) = @_;

    # read cli paramter
    $self->_parse_options;

    # read rc file if activated
    !$self->{CONFIG}->{no_rc_file} && $self->_read_rc_file;

    # use cache if specified
    $self->{CONFIG}->{use_cache} &&  $self->_read_cache;

    # parse `port outdated` if cache is not used or the cache is not valid anymore
    if (!$self->{CONFIG}->{use_cache} || !$self->{CACHE}) {
        $self->_parse_port_outdated;
    }

    # if the cache is valid, use it!
    if ($self->{CONFIG}->{use_cache} && $self->{CACHE}) {
        $self->{OUTDATED} = clone $self->{CACHE};
    }

    $self->{CONFIG}->{use_cache} && $self->_write_cache;

    # add config and found ports to the debug messages
    $self->{CONFIG}->{debug}
        && push @{$self->{DEBUG_MSGS}}, Data::Dumper->Dump(
            [ $self->{CONFIG}, $self->{OUTDATED} ],
            [ qw(config outdated) ]
        );

    # output debug messages if requested
    if ($self->{CONFIG}->{debug}) {
        for my $msg (@{$self->{DEBUG_MSGS}}) {
            say colored([ qw(bright_blue) ], $msg);
        }
    }

    # define some shorthands for laziness
    my ($warn_outdated_defs, $outdate_defs, $outdated_count, $quiet) = (
        $self->{CONFIG}->{warn_about_outdated_defs},
        $self->{OUTDATED}->{decoded}->{definitions},
        $self->{OUTDATED}->{decoded}->{count},
        $self->{CONFIG}->{quiet},
    );

    # echo only when not silenced (not quiet is the default)
    if (!$quiet) {
        if ($warn_outdated_defs && $outdate_defs) {
            say colored([ qw(red) ], q{Definitons must be updated!});
        }

        if ($outdated_count) {
            say colored([ qw(red) ], sprintf q{%d packages for update available!}, $outdated_count );
        } else {
            say colored([ qw(green) ], q{No packages for update available!});
        }
    }

    # again, laziness
    my $exit_codes = $self->{CONFIG}->{exit_codes};
    exit $exit_codes->{only_ports_outdated} if (!$outdate_defs && $outdated_count);
    exit $exit_codes->{only_defs_outdated}  if ($outdate_defs  && !$outdated_count);
    exit $exit_codes->{both_outdated}       if ($outdate_defs  && $outdated_count);

    return;
}

=head1 SYNOPSIS

=cut
sub _parse_options {
    my $self = shift;

    # because Getopt::Long::GetOptions does not stop on die or croak, we
    # have to fetch errors in a array and print them later
    my @errors;

    # save argv for convenience
    $self->{CONFIG}->{argv} = [ @ARGV ];

    # configure Getopt::Long:
    #   no_debug            Don't output debug information
    #   no_auto_abbrev      Don't automatically create abbreviations
    #   bundling            Allow bundling: -r -l -->  -rl
    #   no_auto_version     Don't handle --version automatically
    #   no_auto_help        Don't handle --help automatically
    Getopt::Long::Configure qw( no_debug no_auto_abbrev no_bundling no_auto_version no_auto_help);

    # now the real monty
    GetOptions(
        q{config|c=s}                => sub {
            my ($arg_name, $arg_value) = @_;

            if (!-e Cwd::abs_path($arg_value)) {
                push @errors, qq{Can't read a config from '$arg_value'. File does not exist.};
            }

            $self->{CONFIG}->{config_file} = $arg_value;
        },
        q{rc!}                       => \$self->{CONFIG}->{no_rc_file},
        q{cache!}                    => \$self->{CONFIG}->{use_cache},
        q{cachefile|r=s}             => \$self->{CONFIG}->{cache_file},
        q{verbose|v!}                => \$self->{CONFIG}->{verbose},
        q{quiet!}                    => \$self->{CONFIG}->{quiet},
        q{debug!}                    => \$self->{CONFIG}->{debug},
        q{port-executable|p=s}       => \$self->{CONFIG}->{port_executable},
        q{warn-about-outdated-defs!} => \$self->{CONFIG}->{warn_about_outdated_defs},
        q{cache_expire_days|d=i}     => \$self->{CONFIG}->{cache_expire_days},
    );

    # if something is left in @ARGV, bail
    if (@ARGV) {
        pod2usage(
            -msg        =>  q{Unparseable arguments received: } . join q{,}, @ARGV,
            -exitval    => 2,
        );
    }

    # after all, croak / die on errors
    if (@errors) {
        pod2usage(
            -msg => join( qq{\n}, (q{There were errors: }, @errors)),
            -exitval => 2,
        );
    }

    return;
}

sub _read_cache {
    my $self = shift;

    # read from cachefile, but do not bail even if the file does not exist or couldn't b read
    my $eval_return = eval{ $self->{CACHE}->{raw} = File::Slurp::read_file($self->{CONFIG}->{cache_file}) };

    # but grumble about errors if asked for
    if ($self->{CONFIG}->{verbose} && !$eval_return) {
        chomp $EVAL_ERROR;
        carp colored([qw(red)], $EVAL_ERROR);
    };

    if ($self->{CACHE}->{raw}) {
        $eval_return = eval { $self->{CACHE}->{decoded} = decode_json($self->{CACHE}->{raw}) };

        if (!$eval_return) {
            chomp $EVAL_ERROR;
            carp colored([qw(red)], $EVAL_ERROR);
        }
    }

    if ($self->{CONFIG}->{debug}) {
        push @{$self->{DEBUG_MSGS}}, Data::Dumper->Dump( [ $self->{CACHE}->{raw} ], [qw( Cache_Raw_JSON )] );
        push @{$self->{DEBUG_MSGS}}, Data::Dumper->Dump( [ $self->{CACHE}->{decoded} ], [qw( Cache_Decoded )] );
    }

    # create timestamp for when the cache would be invalid
    my $cache_expiry_ts = DateTime
        ->now()
        ->subtract( days => $self->{CONFIG}->{cache_expire_days});

    $self->{CONFIG}->{debug}
        && push @{$self->{DEBUG_MSGS}}, Data::Dumper->Dump( [ $cache_expiry_ts->epoch ], [qw( cache_exp_ts )] );

    if ($self->{CONFIG}->{verbose}) {
        say {*STDERR} colored(
            [qw{yellow}],
            sprintf
                q{Cache creation dates before %s are considered to old. Cache dates from %s.},
                scalar localtime $cache_expiry_ts->epoch,
                scalar localtime $self->{CACHE}->{decoded}->{time}
        );
    }

    if ($self->{CACHE}->{decoded}->{time} < $cache_expiry_ts->epoch) {
        $self->{CACHE} = undef;
        say {*STDERR} colored([qw{yellow}], q{Cache is not valid anymore.});
    }

    return;
}

sub _read_rc_file {
    my $self = shift;
    my $yml;

    # read from config file, but do not bail even if the file does not exist or couldn't b read
    my $eval_return = eval { $yml = File::Slurp::read_file($self->{CONFIG}->{config_file}) };

    # but grumble about errors if asked for
    ($self->{CONFIG}->{verbose} && !$eval_return) && say colored([qw(red)], $EVAL_ERROR);

    # process
    my ($read_config_vars);
    if ($yml) {
        $eval_return = eval { $read_config_vars = YAML::Load($yml); }
    }

    ($self->{CONFIG}->{verbose} && !$eval_return)
        && say colored([qw{red}], $EVAL_ERROR);

    # overwrite config vars from config file
    if ($read_config_vars) {
        for my $var (keys %{$read_config_vars}) {
            if (defined($self->{CONFIG}->{$var})) {
                # no change made, skip
                next if $self->{CONFIG}->{$var} eq $read_config_vars->{$var};

                # notify about changes
                $self->{CONFIG}->{verbose}
                    && say {*STDERR} colored(
                            [qw(yellow)],
                            qq(Replaced ${var}'s value '$self->{CONFIG}->{$var}' with '$read_config_vars->{$var}' from config file)
                    );
                $self->{CONFIG}->{$var} = $read_config_vars->{$var};
                next;
            }

            # but bail, if asked and a config variable is not set yet (so it's a foreign one)
            $self->{CONFIG}->{verbose}
                && carp colored([qw(red)], qq{$var is not a valid config variable});
        }

        # debug: show updated $self->{CONFIG}
        $self->{CONFIG}->{debug}
            && push @{$self->{DEBUG_MSGS}}, Data::Dumper->Dump( [ $self->{CONFIG} ], [qw( config_after_rc_file)] );
    }

    $self->{CONFIG}->{debug}
        && push @{$self->{DEBUG_MSGS}}, Data::Dumper->Dump( [$yml, $read_config_vars], [qw( raw_yaml parsed_yaml )] );

    return;
}

sub _parse_port_outdated {
    my $self = shift;

    if (! -x $self->{CONFIG}->{port_executable}) {
        croak qq{$self->{CONFIG}->{port_executable} is not executable};
    }

    my ($stdout, $stderr, $exit, $outdated_defs, @outdated_packages);

    # capture output from `port outdated` into $stdout, a warning about outdated
    #   defs will be put to $stderr and the exit code will go to $exit
    my $eval_return = eval {
        ($stdout, $stderr, $exit) = Capture::Tiny::capture {
            system $self->{CONFIG}->{port_executable}, qw( outdated );
        };
    };

    # something went wrong
    if (!$eval_return) {
        croak $eval_return;
    }

    # some other thing went wrong
    if ($exit) {
        croak $stderr;
    }

    # get rid of some newlines
    chomp $stdout;
    chomp $stderr;

    # populate flag for outdated definitions
    if ( $stderr =~ qr{.*port.definitions.are.more.than.two.weeks.old.*}ixms ) {
        $outdated_defs = 1;
    }

    # pre-compile field separator regex for port outdated line
    ## no critic (RegularExpressions::ProhibitComplexRegexes)
    my $field_separator_re = qr{
        ^                                   # m flag will tread every line as new regex
        (?<port>[\S]+)                      # first field: port name
        [\s]+                               # devided by blanks 'n stuff
        (?<current_version>[\S]+)           # second field: current version
        \s\<\s                              # devided by ' < '
        (?<new_version>[\S]+)               # third field: new version
        .*                                  # ignore the rest
        $                                   # until end of line
    }xms;
    ## critic

    # split output from port into a more useable data structure:
    #  @outdated_packages = [
    #       [ $port_name, $current_version, $old_version ],
    #       [...]
    #  ]
    for my $line (split qr{\n}xms, $stdout) {
        my ($port, $current_version, $new_version);

        # next for headline
        next if ($line =~ qr{The.following.installed.}xms);

        if ($line =~ $field_separator_re) {
            $port               = $LAST_PAREN_MATCH{port};
            $current_version    = $LAST_PAREN_MATCH{current_version};
            $new_version        = $LAST_PAREN_MATCH{new_version};

            push @outdated_packages, [ $port, $current_version, $new_version];
            next;
        }

        carp colored([qw{red}], qq{Parsing error in line '$line'. Skipped.});
    }

    $self->{OUTDATED}->{decoded} = {
        count       => scalar @outdated_packages,
        packages    => \@outdated_packages,
        time        => time,
        definitions => $outdated_defs,
    };
    
    $self->{CONFIG}->{debug} &&
        push @{$self->{DEBUG_MSGS}}, 
            Data::Dumper->Dump( [ $self->{OUTDATED} ], [qw( self_outdated_after_readout )] );

    $self->{OUTDATED}->{raw} = encode_json($self->{OUTDATED}->{decoded});

    $self->{CONFIG}->{verbose}
        && say {*STDERR} colored([qw{green}], qq{$self->{OUTDATED}->{decoded}->{count} packages found!});

    $self->{CONFIG}->{verbose}
        && say {*STDERR} colored([qw{green}], qq{Generated JSON:\n$self->{OUTDATED}->{raw}});

    return;
}

sub _write_cache {
    my ($self) = shift;

    $self->{CONFIG}->{verbose} &&
        push @{ $self->{DEBUG_MSGS} }, Data::Dumper->Dump( [ $self->{CACHE}->{valid}, $self->{CONFIG}->{use_cache} ], [qw( cache_is_valid use_cache  )] );

    # only write cache if not longer valid, cache is used and we have something to write
    if (!$self->{CACHE}->{valid} && $self->{CONFIG}->{use_cache} && defined $self->{OUTDATED}->{raw}) {
        File::Slurp::overwrite_file(
            $self->{CONFIG}->{cache_file},
            $self->{OUTDATED}->{raw}
        );

        $self->{CONFIG}->{verbose} &&
            say colored([ qw{ yellow } ], qq{Cache written to $self->{CONFIG}->{cache_file}});
    }

    return;
}

1;