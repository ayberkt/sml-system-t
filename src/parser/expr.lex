structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token
fun getstring s =
   let
    val n = size s
   in
     substring (s, 1, n-2)
   end

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)

exception Illegal_character of pos

%%
%header (functor Exp_LexFun(structure Tokens: Exp_TOKENS));

alpha=[A-Za-z];
digit=[0-9];
any = [@a-zA-Z0-9];

ws = [\ \t];

%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
"z"      => (Tokens.ZERO(!pos, !pos));
"s"      => (Tokens.SUCC(!pos, !pos));
":"      => (Tokens.COLON(!pos,!pos));
"nat"    => (Tokens.NATTYPE(!pos,!pos));
"rec"    => (Tokens.REC(!pos, !pos));
"with"   => (Tokens.WITH(!pos, !pos));
"=>"     => (Tokens.MAPSTO(!pos, !pos));
"->"     => (Tokens.ARRTYPE(!pos, !pos));
"|"      => (Tokens.BAR(!pos, !pos));
"{"      => (Tokens.LCURLY(!pos, !pos));
"}"      => (Tokens.RCURLY(!pos, !pos));
"\\"     => (Tokens.LAMBDA(!pos, !pos));
"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));
{alpha}{any}* => (Tokens.IDENT(yytext,!pos,!pos));
