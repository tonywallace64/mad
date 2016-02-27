-module(mad_compile_SUITE).

-export([all/0]).
-export([erl_files/1]).
-export([app_src_files/1]).
-export([deps/1]).
-export([app/1]).
-export([is_compiled/1]).

-import(helper, [get_value/2]).


all() ->
    [
     erl_files, app_src_files, deps,
     app, is_compiled
    ].

erl_files(Config) ->
    DataDir = get_value(data_dir, Config),
    SrcDir = filename:join([DataDir, "deps", "one", "src"]),
    ErlFile = filename:join(SrcDir, "one.erl"),
    {ok,_}=mad_compile:compile([SrcDir]).

app_src_files(Config) ->
    DataDir = get_value(data_dir, Config),
    SrcDir = filename:join([DataDir, "deps", "one", "src"]),
    AppSrcFile = filename:join(SrcDir, "one.app.src"),
    {ok,_}=mad_compile:compile(AppSrcFile).

deps(Config) ->
    DataDir = get_value(data_dir, Config),
    Deps = [{one, "", {}}, {two, "", {}}],
    {ok,deps} = mad_compile:deps(DataDir, Config, "rebar.config", Deps),
    pong = one:ping(),
    pong = two:ping(),
    ok = application:load(one),
    ok = application:load(two),
    {ok, [one]} = application:get_key(one, modules),
    {ok, [two]} = application:get_key(two, modules),

    ok = one:test_inc_hrl(),
    ok = one:test_src_hrl(),
    ok = two:test_inc_hrl(),
    ok = two:test_src_hrl().

app(Config) ->
    DataDir = get_value(data_dir, Config),
    {ok,_} = mad_compile:compile(["rebar.config"]),
    pong = three:ping(),
    ok = application:load(three),
    {ok, [three]} = application:get_key(three, modules),
    ok = three:test_inc_hrl(),
    ok = three:test_src_hrl().

is_compiled(Config) ->
    DataDir = get_value(data_dir, Config),
    SrcDir = filename:join([DataDir, "deps", "one", "src"]),
    EbinDir = filename:join([SrcDir, "..", "ebin"]),
    BeamFile1 = filename:join(EbinDir, "x.beam"),
    BeamFile2 = filename:join(EbinDir, "one.beam"),
    false = mad_compile:is_compiled(BeamFile1, filename:join(SrcDir, "x.erl")),
    true = mad_compile:is_compiled(BeamFile2, filename:join(SrcDir, "one.erl")).
