-module(mad_script).
-copyright('Sina Samavati').
%-compile(export_all).
-export([script/3]).

script(ConfigFile, Conf, _) ->
    File = ConfigFile ++ ".script",
    case file:script(File, [{'CONFIG', Conf}, {'SCRIPT', File}]) of
        {ok, {error,_}} -> Conf;
        {ok, Out} -> Out;
        {error, _} -> Conf
    end.
