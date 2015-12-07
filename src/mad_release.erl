-module(mad_release).
-version("1.0.1").
-export([release/1]).
-author("Tony Wallace").

%% with yecc parser the first parameter of the list will be
%% an atom, and there will be two parameters.  Other options,
%% left for now for backwards compatability.

%% retained for backwards compatability 
release([])              -> release(["script"]);
release(["depot"])       -> release([depot, "sample"]);
release(["beam"])        -> release([beam,  "sample"]);
release(["ling"])        -> release([ling,  "sample"]);
release(["script"])      -> release([script,"sample"]);
release([X,N]) when is_list(X) -> release([list_to_atom(X),N]);
%% end of backwards compatability

%% new style retain...
release([ling,N])      -> mad_ling:ling(N);
release([script,N])    -> mad_escript:main(N);
release([beam,N])      -> mad_systools:beam_release(N);
release([X])             -> release([script,X]).
