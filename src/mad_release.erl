-module(mad_release).
-version("1.0.1").
-export([release/1]).
-author("Tony Wallace").

%% with yecc parser the first parameter of the list will be
%% an atom, and there will be two parameters.  Other options,
%% left for now for backwards compatability.

%% retained for backwards compatability 
release([])              -> release(["script"]);
release(["beam"])        -> release([beam,  "sample"]);
release(["ling"])        -> release([ling,  "sample"]);
release(["script"])      -> release([script,"sample"]);
release([X,N]) when is_list(X) -> release([list_to_atom(X),N]);
%% end of backwards compatability

%% new style retain...
release([ling,N])      -> ling(N);
release([script,N])    -> script(N);
release([beam,N])      -> systools(N);
release([X])             -> release([script,X]).

%% AJW Jan 2016 added configurable release hook
ling(N) ->
    before_release(),
    mad_ling:ling(N).

script(N) ->
    before_release(),
    mad_escript:main(N).

systools(N) ->
    before_release(),
    mad_systools:beam_release(N).

before_release() ->
    io:format("mad release executing before release~n"),
    {Cwd,ConfigFile,Conf} = mad_utils:configs(),
    {M,F,A} = mad_utils:get_value(before_release,Conf,{mad_release,not_set,[ConfigFile]}),
    M:F(A).

not_set(ConfFile) ->
    Cwd = mad_utils:cwd(),
    mad:info("No before release handler set in ~s~n",[filename:join(Cwd,ConfFile)]),
    mad:info("To set use property {before_release,{Module,Function,[Args]}}~n").
    
