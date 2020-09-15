#!/usr/bin/perl
# prepare $ENV
BEGIN {
    $ENV{HOME} = q{/tmp};
}
use strict;
use warnings;
use Test2::V0 q{:DEFAULT};
use Test2::Tools::Compare;
use Test2::Tools::Basic;
use lib 'lib';
use MacPorts::Outdated::Notify;
use Data::Dumper;
use File::Util qw{SL};
use v5.20;

my ($default_object_instance, $modified_object_instance);
my $default_config = {
    valid                    => 0,
    use_cache                => 1,
    no_rc_file               => 0,
    verbose                  => 0,
    quiet                    => 0,
    config_file              => $ENV{HOME} . SL . q{.portcheck.yml},
    cache_file               => $ENV{HOME} . SL . q{.portcheck.cache}, #
    cache_expire_days        => 7,
    warn_about_outdated_defs => 1,
    port_executable          => q{/opt/local/bin/port},
    exit_codes               => {
        'all_good'            => 0,
        'general_error'       => 1,
        'only_ports_outdated' => 200,
        'only_defs_outdated'  => 201,
        'both_outdated'       => 202,
    },
    debug                       =>0,
};

my $default_cache = {
    valid => 0,
};

$default_object_instance = MacPorts::Outdated::Notify->new;
$modified_object_instance = MacPorts::Outdated::Notify->new;

subtest q{Generate new instance} => sub {
    is(ref $default_object_instance, q{MacPorts::Outdated::Notify}, q{New object instance created.});
    is($default_object_instance->get_outdated, F(), q{MacPorts::Outdated::Notify::outdated is empty});
    is($default_object_instance->get_config, $default_config, q{Default config was set});
    is($default_object_instance->get_cache, $default_cache, q{Default cache was set});
};

subtest q{Test getConfig} => sub {
    is(ref $default_object_instance->get_config, q{HASH}, q{getConfig without key returns all the config vars});

    {
        $default_object_instance->get_config(q{notset});
        is($default_object_instance->error, q{Parameter given, but does not exist}, q{getConfig(notset) will return an error});
    }
};

subtest q{Instances are not interdependent} => sub {
    {
        $modified_object_instance->set_config(q{foo}, q{bar});
        is($default_object_instance->get_config(q{foo}), F(), q{Get config 'foo' from unmodified instance will fail});
        is($default_object_instance->error, q{Parameter given, but does not exist}, q{getConfig(foo) on unmodified instance will return an error});
    }
};

done_testing();

say Data::Dumper->Dump( [ $default_object_instance, $modified_object_instance ], [qw( first_inst second_inst )] )

