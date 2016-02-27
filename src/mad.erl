-module(mad).
-author("Tony Wallace").
-version("1.0.1").
-include("mad.hrl").
%-compile(export_all).
-export([main/1,profile/0,help/0,help/1,help/2,info/1,info/2,parser_test/0]).

%% 18 October 2015
%%   Code simplified and commented by Tony Wallace (AJW)
%% 3 November 2015
%%   -export declaration
%%   Passes selfcompile test
%% 5 December
%%   factor out lexical analysis and parsing
%%   parsing with yecc

-type escript_parameters() :: [string()].
-spec main(escript_parameters()) -> any().

parser_test() ->
    io:format("~p~n~n",[{ok,[{deps,[]}]} = mad_parser:parse([{cmd,1,deps},{eof,1}])]),
    io:format("~p~n~n",[{ok,[{release,[script,"mymad"]}]} =
	mad_parser:parse([{release,1},{rel_opt,1,script},{parameter,1,"mymad"},{eof,1}])]),
    io:format("~p~n~n",[{ok,[{deps,[]},{release,[script,"mymad"]}]} =
	mad_parser:parse([{cmd,1,deps},{release,2},{rel_opt,2,script},{parameter,2,"mymad"},{eof,2}])]),
    io:format("~p~n~n",[{ok,[{deps,[]}]} = mad_parser(["deps"])]),
    {ok,[{deps,[]},{release,[script,"mymad"]}]}=mad_parser(["deps","release","script","mymad"]),
    {error,_}=mad_parser(["release"]),
    {error,_}=mad_parser(["release","script"]),
    {error,_}=mad_parser(["release","mymad"]),
    "Tests passed okay".

main([])          -> help();
main(Params)      ->
    Parsed = mad_parser(Params),
    io:format("parse output ~p~n",[Parsed]),
    maybe_execute_all(Parsed).

maybe_execute_all(A={error,_}) ->
    A;
maybe_execute_all({ok,Valid}) ->
    execute_all(Valid).

-spec mad_parser(escript_parameters()) -> parser_output().
-type parser_output() :: command_list().
-type command_list() :: [{Command :: atom(),escript_parameters()}].
mad_parser(Parameters) ->
    Lex=madscan:scan(Parameters),
    mad_parser:parse(Lex).
    
-spec execute_all(parser_output()) -> any().
execute_all(Valid) ->
    %% Run commands in order of original command line,
    %% using the command mapping (Dispatcher) as referenced in the "profile" application 
    %% environment variable.  The value of the environment variable is 
    %% an atom that is the name of an erlang module that maps commands
    %% to erlang calls.  See mad_local.erl for an example.
    %% See erlang_otp for documentation on application environment variables.
    %% When that variable is unset use mad_local.erl profile.
    %% Process in return values from the command with errors procedure,
    %% and return boolean, true if errors false if no errors.
    Dispatcher = profile(),
    execute2(ok,Dispatcher,Valid).

execute2(ok,Dispatcher,[{Fun,Arg}|ToDoList]) ->
    io:format("Dispatching ~p with ~p~n",[Fun,Arg]),
    execute2(
      errors((Dispatcher):Fun(Arg)),Dispatcher,ToDoList);
execute2(error,_,_) -> 
    return(error);
execute2(ok,_,[]) ->
    return(ok).

profile()         -> application:get_env(mad,profile,mad_local).

errors([])        -> ok;
errors(ok)        -> ok;
errors(false)     -> ok;
errors(true)      -> error;
errors({error,L}) -> info("ERROR: ~tp~n",[L]), error;
errors({ok,_})    -> info("OK~n",[]), ok;
errors(X)         -> info("RETURN: ~tp~n",[X]), X.

return(error)      -> 1;
return(ok)         -> 0;
return(X)          -> X.

format_stack_entry(S) ->
    {Module,Fun,Arity,[{file,_File},{line,Line}]}=S,
    io_lib:format("~p:~p/~p,{line,~p} ==>",[Module,Fun,Arity,Line]).
stacktop([Top|_]) ->
    Top.
ancestor(N) ->
    {_,Stacktrace}=erlang:process_info(self(),current_stacktrace),
    ancestor(N+1,Stacktrace).
ancestor(1,S) ->
    format_stack_entry(stacktop(S));
ancestor(N,[_|T]) ->
    ancestor(N-1,T).

info(Format)      -> io:format(lists:concat([ancestor(2),Format,"\r"])).
info(Format,Args) -> io:format(lists:concat([ancestor(2),Format,"\r"]),Args).

help(Reason,D)    -> help(io_lib:format("~s ~p", [Reason, D])).
help(Msg)         -> help("Message",Msg).
help()            -> info("MAD Container Tool version ~s~n",[?VERSION]),
                     info("~n"),
                     info("    invoke = mad params~n"),
                     info("    params = [] | command [ options  ] params ~n"),
                     info("   command = app     | deps  | clean | compile | up~n"),
                     info("           | release [ beam  | ling  | script  | runc | depot ]~n"),
                     info("           | deploy  | start | stop  | attach  | sh ~n"),
                     return(false).
