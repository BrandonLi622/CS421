signature extra_TOKENS =
sig
type ('a,'b) token
type svalue
val W5:  'a * 'a -> (svalue,'a) token
val W4:  'a * 'a -> (svalue,'a) token
val W3:  'a * 'a -> (svalue,'a) token
val W2:  'a * 'a -> (svalue,'a) token
val W1:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature extra_LRVALS=
sig
structure Tokens : extra_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
