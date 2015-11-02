-module(mad).
-author("Tony Wallace").
-version("1.0.0").
-include("mad.hrl").
%-compile(export_all).
-export([main/1,profile/0,help/1,help/2,info/2]).

%% 18 October 2015
%%   Code simplified and commented by Tony Wallace (AJW)
%% 3 November 2015
%%   -export declaration
%%   Passes selfcompile test

main([])          -> help();
main(Params)      ->
    io:format("mad:main(~p)~n",[Params]),
    Parsed = mad_parser(Params),
    io:format("parse output ~p~n",[Parsed]),
    execute_all(Parsed).

mad_parser(Params) ->
    %% Comments by AJW
    %% This next expression parses the list Params,
    %% from right to left (foldr),
    %% accumulating string parameters in order in the first tuple position
    %% identifying commands because they have been atomised
    %% and then assembling these commands in order with their parameters
    %% in a list stored in the second tuple position

    %% commands processed by mad are introduced by a token
    %% atomize turns these predefined tokens into atoms
    Lex = lists:map(fun atomize/1, Params),

    EmptyStack = [],
    PushParameter = fun(Stack,P) -> [P|Stack] end,
    %% This function pushes string parameters onto a stack
    %% and associates this stack with its command.
    %% This works because the mad parameters are processed right to left
    ProcessSymbol  =
               fun (Command,{PStack,ParsedCommands}) when is_atom(Command) -> 
		       {EmptyStack,[{Command,PStack}|ParsedCommands]};
                   (Param,{PStack,Remaining}) -> 
		       {PushParameter(PStack,Param),Remaining} end,
    { _UnmatchedStrings, _CommandList } 
	 =  lists:foldr(ProcessSymbol,  {EmptyStack,[]}, Lex).

execute_all({[],Valid}) ->
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
    execute2(ok,Dispatcher,Valid);
execute_all({Unmatched_Params,_}) ->
    errors(Unmatched_Params),
    return(error).

execute2(ok,Dispatcher,[{Fun,Arg}|ToDoList]) ->
    io:format("Dispatching ~p with ~p~n",[Fun,Arg]),
    execute2(
      errors((Dispatcher):Fun(Arg)),Dispatcher,ToDoList);
execute2(error,_,_) -> 
    return(error);
execute2(ok,_,[]) ->
    return(ok).

atomize("static") -> 'static';
atomize("deploy") -> 'deploy';
atomize("app"++_) -> 'app';
atomize("dep")    -> 'deps';
atomize("deps")   -> 'deps';
atomize("cle"++_) -> 'clean';
atomize("com"++_) -> 'compile';
atomize("up")     -> 'up';
atomize("rel"++_) -> 'release';
atomize("bun"++_) -> 'release';
atomize("sta"++_) -> 'start';
atomize("sto"++_) -> 'stop';
atomize("att"++_) -> 'attach';
atomize("sh")     -> 'sh';
atomize("rep"++_) -> 'sh';
atomize("pla"++_) -> 'release';
atomize(Else)     -> Else.

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

info(Format)      -> io:format(lists:concat([Format,"\r"])).
info(Format,Args) -> io:format(lists:concat([Format,"\r"]),Args).

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
