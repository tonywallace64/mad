Nonterminals command_string command command_sym option_sym parameter_sym cmd_opts cmd_opt.
Terminals parameter cmd release opt eop rel_opt.
Rootsymbol command_string.
Endsymbol eof.
command_string -> command  : '$1'.
command -> command_sym cmd_opts command : [{'$1','$2'}|'$3'].
command ->
    release rel_opt parameter command : [{ release , [unwrap('$2'), unwrap('$3')]} | '$4'].
%command ->
%    eof : [].
command ->
   '$empty' : [].
cmd_opts ->
    cmd_opt cmd_opts : ['$1' | '$2'].
cmd_opts ->
   '$empty' : [].
cmd_opt ->
    option_sym : '$1'.
cmd_opt ->
    parameter_sym : 
        '$1'.
command_sym -> cmd 
	    : 		   
	       V=unwrap('$1'),
	       V.
option_sym ->
    opt : unwrap('$1').
parameter_sym ->
    parameter : unwrap('$1').
Erlang code.

unwrap({_,_,V}) -> V.
