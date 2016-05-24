signature Exp_TOKENS =
sig
type ('a,'b) token
type svalue
val SEMI:  'a * 'a -> (svalue,'a) token
val UNCHECKED:  'a * 'a -> (svalue,'a) token
val CHECKED:  'a * 'a -> (svalue,'a) token
val EVAL:  'a * 'a -> (svalue,'a) token
val STEP:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val WITH:  'a * 'a -> (svalue,'a) token
val BAR:  'a * 'a -> (svalue,'a) token
val RCURLY:  'a * 'a -> (svalue,'a) token
val LCURLY:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val MAPSTO:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val LAMBDA:  'a * 'a -> (svalue,'a) token
val REC:  'a * 'a -> (svalue,'a) token
val SUCC:  'a * 'a -> (svalue,'a) token
val ZERO:  'a * 'a -> (svalue,'a) token
val ARRTYPE:  'a * 'a -> (svalue,'a) token
val NATTYPE:  'a * 'a -> (svalue,'a) token
val IDENT: (string) *  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Exp_LRVALS=
sig
structure Tokens : Exp_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
