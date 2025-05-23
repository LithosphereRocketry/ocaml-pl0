{
(* header *)
open Parser
}
rule token = parse
        [' ' '\t' '\r' '\n']+    { token lexbuf } (* skip blanks *)
{
(* trailer *)
}
