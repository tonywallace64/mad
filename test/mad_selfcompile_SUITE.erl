-module(mad_selfcompile_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0,init_per_suite/1,end_per_suite/1]).
-export([selfcompile/1]).

%% mad is compiled into an escript
%% This test uses a known good copy of mad
%% to compile the System Under Test
%% The resulting script is then used to compile
%% the system under test which should compile okay
all() ->
    [selfcompile].
init_per_suite(Config) ->
    PrivDir=get_value(priv_dir,Config),
    DataDir=get_value(data_dir,Config),
    Fn=filename:join([DataDir,"TestConfig"]),
    {ok,UserConfig} = file:consult(Fn),
    assert(is_property_list(UserConfig),"TestConfig does not return a valid property list"),
    NewConfig = Config ++ UserConfig,
    SrcDir=get_value(srcdir,NewConfig),
    copy_files(SrcDir,filename:absname("."),{recursive,false}),
    copy_files(filename:join(SrcDir,"src"),filename:absname("src"),{recursive,true}),
    copy_files(filename:join(SrcDir,"deps"),filename:absname("deps"),{recursive,true}),
    copy_files(filename:join(SrcDir,"include"),filename:absname("include"),{recursive,true}),
    % do_valid_cmd("cp --recursive "++filename:join(SrcDir,"src")++" ./src"),
    % do_valid_cmd("cp --recursive "++filename:join(SrcDir,"deps")++" ./deps"),
    % do_valid_cmd("cp --recursive "++filename:join(SrcDir,"include")++" ./include"),
    do_valid_cmd("mkdir ebin"),
    do_valid_cmd("make"),
    NewConfig.

copy_files(SrcDir,DstDir,{recursive,Recursive}) ->
    ct_logs:tc_pal(file_copy,"Current Working directory~n~s",[filename:absname(".")]),
    case file:make_dir(DstDir) of
	ok -> ok;
	{error,eexist} -> ok
    end,
    {ok,Files}=file:list_dir(SrcDir),
    copy_filelist(Files,SrcDir,DstDir,Recursive).

copy_filelist([],_,_,_) ->
    ok;
copy_filelist([H|T],SrcDir,DstDir,Recursive) ->
    Pn = filename:join([SrcDir,H]),
    copy_head(filelib:is_regular(Pn),filelib:is_dir(Pn),Recursive,Pn,DstDir),
    copy_filelist(T,SrcDir,DstDir,Recursive).

copy_head(true,false,_,H,DstDir) ->
    DstFile = filename:join(DstDir,filename:basename(H)),
    ct_logs:tc_pal(file_copy,"~s ~s",[H,DstFile]),
    {ok,_}=file:copy(H,DstFile),
    {ok,Fi}=file:read_file_info(H),
    ok=file:write_file_info(DstFile,Fi);
copy_head(false,true,false,_,_) ->
    ok;
copy_head(false,true,true,Dirname,DstDir) ->
    NewDst = filename:join(DstDir,filename:basename(Dirname)),
    copy_files(Dirname,NewDst,{recursive,true}).

do_valid_cmd(Cmd) ->
    ct_logs:tc_pal(os_cmd,"~s",[Cmd]),
    Result=os:cmd(Cmd ++ " && echo ok"),
    SR=lists:reverse(string:tokens(Result,"\n\r")),
    valid_cmd(Cmd,Result,SR).
valid_cmd(Cmd,[],[]) ->
    ct_logs:tc_pal(os_cmd_error,"~ts",[Cmd]),
    error("Command returned no info");
valid_cmd(_,_,["ok"|_]) ->
    ct_logs:tc_pal(os_cmd,"  ok~n",[]),
    ok;
valid_cmd(Cmd,Result,_) ->
    Msg = lists:flatten(Cmd++"/n"++Result),
    ct_logs:tc_pal(os_cmd_error,"~ts",[Msg]),
    error(Msg).

end_per_suite(_Config) -> ok.

selfcompile(_) ->
    {ok,_}=file:copy("oldmad","mad"),
    %% return status deliberately not checked for successful deletion newmad, newmad2
    %% it is possible that they do not exist which would be fine
    file:delete("newmad"),
    file:delete("newmad2"),
    %% having just been deleted these files should not exist,  check.
    false = filelib:is_regular("newmad"),
    false = filelib:is_regular("newmad2"),
    do_valid_cmd("./mad release script newmad"),
    ok=file:delete("mad"),
    ok=file:rename("newmad","mad"),
    do_valid_cmd("./mad release script newmad2"),
    true = filelib:is_regular("newmad2").

is_property_list([]) ->
    true;
is_property_list([{_Key,_Value}|Rest]) ->
    is_property_list(Rest).

assert(true,_) ->
    ok;
assert(ok,_) ->
    ok;
assert(T,_) when is_tuple(T) andalso ok=:=element(1,T) ->
    ok;
assert(_,Msg) ->
    error("assertion failed: "++Msg).


get_value(Key,PropList) ->    
    valid_key(Key,proplists:get_value(Key,PropList)).
valid_key(Key,undefined) ->
    error(io_lib:format("undefined configuration key ~p",[Key]));
valid_key(_,Value) -> Value.

