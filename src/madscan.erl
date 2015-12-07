%% implement a scanner
%% compatible with yecc generated parser
%% some parser logic here to allow yecc parser to operate correctly
-module (madscan).
-export([scan/1,test/0]).

-type escript_parameters() :: [string()].
-type lex() :: {atom(),integer(),atom()|string()}|{atom(),integer()}.
-type lexical_parameters() :: [lex()].
test() ->
    % {error,command_expected} = scan([]),
    [{cmd,1,deps},{eof,1}] = scan(["deps"]),
    [{cmd,1,deps},{cmd,2,compile},{release,3},{parameter,3,"madlex"},{eof,3}] = scan(["deps","comp","release","madlex"]),
    ok.

-spec scan(escript_parameters()) -> lexical_parameters() | {error,any()}.    
scan(Parameters) ->
    Lex1 = lists:map(fun analyse/1, Parameters)++[{eof,1}],
    renum(Lex1,[],0).
    %% for parser reasons, a seperator is placed after each command.
    %% the first entry must be a command
    %separators([],Lex1,0).

renum([],A,_) ->
    lists:reverse(A);
renum([{cmd,_,Cmd}|T],Acc,CmdNum) ->
    renum(T,[{cmd,CmdNum+1,Cmd}|Acc],CmdNum+1);
renum([{release,_}|T],Acc,CmdNum) ->
    renum(T,[{release,CmdNum+1}|Acc],CmdNum+1);
renum([{S,_,V}|T],Acc,CmdNum) ->
    renum(T,[{S,CmdNum,V}|Acc],CmdNum);
renum([{S,_}|T],Acc,CmdNum) ->
    renum(T,[{S,CmdNum}|Acc],CmdNum).

-spec analyse(string()) -> lex().

analyse("static") -> {cmd,1,'static'};
analyse("deploy") -> {cmd,1,'deploy'};
analyse("app"++_) -> {cmd,1,'app'};
analyse("dep")    -> {cmd,1,'deps'};
analyse("deps")   -> {cmd,1,'deps'};
analyse("cle") -> {cmd,1,'clean'};
analyse("clea") -> {cmd,1,'clean'};
analyse("clean") -> {cmd,1,'clean'};
analyse("com") -> {cmd,1,'compile'};
analyse("comp") -> {cmd,1,'compile'};
analyse("compi") -> {cmd,1,'compile'};
analyse("compil") -> {cmd,1,'compile'};
analyse("compile") -> {cmd,1,'compile'};
analyse("up")     -> {cmd,1,'up'};
analyse("rel") -> {release,1};
analyse("rele") -> {release,1};
analyse("relea") -> {release,1};
analyse("releas") -> {release,1};
analyse("release") -> {release,1};
analyse("bun") -> {cmd,1,'release'};
analyse("bund") -> {cmd,1,'release'};
analyse("bundl") -> {cmd,1,'release'};
analyse("bundie") -> {cmd,1,'release'};
analyse("sta") -> {cmd,1,'start'};
analyse("star") -> {cmd,1,'start'};
analyse("start") -> {cmd,1,'start'};
analyse("sto") -> {cmd,1,'stop'};
analyse("stop") -> {cmd,1,'stop'};
analyse("att") -> {cmd,1,'attach'};
analyse("atta") -> {cmd,1,'attach'};
analyse("attac") -> {cmd,1,'attach'};
analyse("attach") -> {cmd,1,'attach'};
analyse("sh")     -> {cmd,1,'sh'};
analyse("rep"++_) -> {cmd,1,'sh'};
analyse("pla"++_) -> {'release',1};

analyse("beam") -> {rel_opt,1,'beam'};
analyse("ling") -> {rel_opt,1,'ling'};
analyse("script") -> {rel_opt,1,script};
analyse("runc") -> {rel_opt,1,runc};
analyse("depot") -> {rel_opt,1,depot};

analyse(Else)     -> {parameter,1,Else}.

