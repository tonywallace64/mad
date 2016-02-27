-module(mad_dtl).
-copyright('Sina Samavati').
-compile(export_all).

compile(Dir,Config) ->
    mad:info("Dir ~s~nConfig ~p~n",[Dir,Config]),
    case mad_utils:get_value(erlydtl_opts, Config, []) of
        [] -> false;
         X -> compile_erlydtl_files(validate_erlydtl_opts(Dir,X)) end.

get_kv(K, Opts, Default) ->
    V = mad_utils:get_value(K, Opts, Default),
    KV = {K, V},
    {KV, Opts -- [KV]}.

file_to_beam(Bin, Filename) -> filename:join(Bin, filename:basename(Filename) ++ ".beam").

validate_erlydtl_opts(Cwd, Opts) ->
    %% AJW
    %% An important function of this routine is to adjust the options
    %% by calculating  absolute paths for the relative paths
    %% in the configuration file

    DefaultDocRoot = filename:join("priv", "templates"),
    {DocRoot, Opts1} = get_kv(doc_root, Opts, DefaultDocRoot),
    {OutDir, Opts2} = get_kv(out_dir, Opts1, "ebin"),
    {CompilerOpts, Opts3} = get_kv(compiler_options, Opts2, []),
    {SourceExt, Opts4} = get_kv(source_ext, Opts3, ".dtl"),
    {ModuleExt, Opts5} = get_kv(module_ext, Opts4, ""),
    {MappingFile, Opts6} = get_kv(dets_map, Opts5, "template_map.dets"),
    {{priv_dir,PrivDir},   Opts7} = get_kv(priv_dir,   Opts6, "priv"),

    {_, DocRootDir} = DocRoot,
    DocRoot1 = {doc_root, filename:join(Cwd, DocRootDir)},
    {_, OutDir1} = OutDir,
    OutDir2 = {out_dir, filename:join(Cwd, OutDir1)},
    PrivDir2={priv_dir, filename:join(Cwd, PrivDir)},

    [DocRoot1, OutDir2, PrivDir2, CompilerOpts, SourceExt, ModuleExt, MappingFile|Opts7].

module_name(File, Ext, NewExt) ->
    list_to_atom(filename:basename(File, Ext) ++ NewExt).

check_detsfile(File) ->
    check_detsfile(dets:is_dets_file(File),File).
check_detsfile(true,_) ->
    true;
check_detsfile(_,_) ->
    false.

compile_erlydtl_files(Opts) ->

    {{_, DocRoot},   Opts1} = get_kv(doc_root,   Opts,  ""),
    {{_, SourceExt}, Opts2} = get_kv(source_ext, Opts1, ""),
    {{_, ModuleExt}, Opts3} = get_kv(module_ext, Opts2, ""),
    {{_, OutDir},    Opts4} = get_kv(out_dir,    Opts3, ""),
    {{_, PrivDir},   _Opts5} = get_kv(priv_dir,   Opts4, "priv"),

    MappingFile = "template_map.dets",
    mad:info("Options file as used:~n~p~n",[Opts]),
    DetsFile = filename:join([PrivDir,MappingFile]),
    Files = filelib:fold_files(DocRoot, SourceExt, true,
                               fun(F, Acc) -> [F|Acc] end, []),

    %% AJW
    %% if mapping file is missing all dtls need to be
    %% recompiled to establish handler/compiled dtl mappings
    %%
    Compile = fun(F) ->
	PageHandler = list_to_atom(filename:basename(F, SourceExt)),
        ModuleName = module_name(F, SourceExt, ModuleExt),
        BeamFile = file_to_beam(OutDir, atom_to_list(ModuleName)),
        Compiled = mad_compile:is_compiled(BeamFile, F),
        case Compiled and check_detsfile(DetsFile) of
	    false ->
		mad:info("DTL Compiling ~s~n", [F -- mad_utils:cwd()]),
		Res = erlydtl:compile(F, ModuleName, Opts3),
		file:change_time(BeamFile, calendar:local_time()),
		case Res of 
		    {error,Error} -> mad:info("Error: ~p~n",[Error]);
                    ok -> {ok,{PageHandler,ModuleName}}
		end;
            true -> ok end
    end,

    CompRes = [Compile(F) || F <- Files],
    %%DetsFile = MappingFile,
    IsMappingData = 
	fun({ok,_X}) -> true;
		(_) -> false
	   end,
    MappingData = [X || X <- CompRes, IsMappingData(X)],
    mad:info("MappingData ~p~n",[MappingData]),
    Pairs = [{PageHdl,Mod} ||{ok,{PageHdl,Mod}} <- MappingData ],
    mad:info("Pairs ~p~n",[Pairs]),
    mad:info("Mapping File: ~s~n",[DetsFile]),
    {ok,TabName} = dets:open_file(DetsFile,[]),
    mad:info("Insert into dets ~p~n",[Pairs]),
    ok = dets:insert(TabName,Pairs),
    ok = dets:close(TabName),
    lists:any(
      fun({error,_}) -> true; 
	 (ok) -> false;
	 ({ok,_}) -> false end,CompRes
      ).
