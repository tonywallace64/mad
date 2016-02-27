-module(mad_escript).
-description("ESCRIPT bundles").
%-compile(export_all).
-export([main/1,privs/0,beams/0,system_files/0]).

main(N) ->
    { _Cwd, _ConfigFile, Conf } = mad_utils:configs(),
    App = filename:basename(case N of [] -> mad_utils:cwd(); E -> E end),
    mad_resolve:main([]),
    EmuArgs = mad_utils:get_value(emuargs,Conf,"+pc unicode"),
    Files = static() ++ beams(fun filename:basename/1, fun read_file/1) ++ overlay(),
    escript:create(App,[shebang,{comment,""},{emu_args,EmuArgs},{archive,Files,[memory]}]),
    file:change_mode(App, 8#764),
    {ok,App}.

% id(X) called by beams/0
id(X) -> X.

% read_file called by beams/2 and main/1
read_file(File) -> {ok, Bin} = file:read_file(filename:absname(File)), Bin.

% static/0 called by main/1
static() ->
    Name = "static.gz",
    {ok,{_,Bin}} = zip:create(Name,
        [ F || F <- mad_repl:wildcards(["{apps,deps}/*/priv/**","priv/**"]), not filelib:is_dir(F) ],
        [{compress,all},memory]),
    [ { Name, Bin } ].

beams() -> beams(fun id/1,  fun read_file/1).
beams(Fun,Read) ->
    [ { Fun(F), Read(F) } ||
        F <- mad_repl:wildcards(["ebin/*","{apps,deps}/*/ebin/*","sys.config",".applist"]) ].

privs() -> privs(fun id/1,  fun read_file/1).
privs(Fun,Read) ->
    [ { Fun(F), Read(F) } ||
        F <- mad_repl:wildcards(["{apps,deps}/*/priv/**","priv/**"]), not filelib:is_dir(F) ].

system_files() -> lists:flatten([system_files(A) || A<- mad_repl:applist(), lists:member(A,mad_repl:system()) ]).
system_files(App) ->
    [ { F, mad_bundle:read_file(F) } ||
        F <- mad_repl:wildcards([lists:concat([code:lib_dir(App),"/ebin/*.{app,beam}"])]) ].

overlay() -> overlay(fun id/1,  fun read_file/1).
overlay(Fun,Read) ->
    [ { Fun(F), Read(F) } ||
        F <- mad_repl:wildcards(["deps/ling/apps/*/ebin/*.beam"]) ].
