type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

exception test;

fun charNumberAsString(s) = Int.toString(Char.ord(hd(String.explode(s))));

(* TODO: Right now this can raise an exception... *)
fun intFromString(s) = valOf(Int.fromString(s));

datatype adjust = INC | DEC;

val commentDepth = ref 0;

fun updateCommentDepth(x) = if x = INC 
                            then commentDepth := (!commentDepth + 1)
                            else commentDepth := (!commentDepth - 1);


(*fun updateState = if commentDepth = 0
                  then YYBEGIN INITIAL
                  else YYBEGIN COMMENT; *)
                  
(*TODO: Can a comment start like /* have stuff in between the / and *? *)
(*TODO: Do I need monads to do strict evaluation ? *)
(*TODO: What about */* or /*/ *)

(*TODO: Is array just a keyword? *)
                            


val eof = fn () => let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%% 
%s COMMENT STRING;

SPACE = [ \t\n\012];
DIGIT = [0-9];
ALPHA = [A-za-z];
ALPNUM = {DIGIT}|{ALPHA};

ID = {ALPHA}({ALPNUM}|_)+;
INT = {DIGIT}+;

COMMSTART = \/\*;
COMMEND   = \*\/;


%%
\n	     =>  (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

<INITIAL> {SPACE}+ =>  (continue());

<INITIAL> var  	   =>  (Tokens.VAR     (yypos,yypos+3));
<INITIAL> type     =>  (Tokens.TYPE    (yypos,yypos+4));
<INITIAL> function =>  (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL> let      =>  (Tokens.LET     (yypos,yypos+3));
<INITIAL> if       =>  (Tokens.IF      (yypos,yypos+2));
<INITIAL> then     =>  (Tokens.THEN    (yypos,yypos+4));
<INITIAL> else     =>  (Tokens.ELSE    (yypos,yypos+4));
<INITIAL> in       =>  (Tokens.IN      (yypos,yypos+2));
<INITIAL> end      =>  (Tokens.END     (yypos,yypos+3));
<INITIAL> while    =>  (Tokens.WHILE   (yypos,yypos+5));
<INITIAL> do       =>  (Tokens.WHILE   (yypos,yypos+2));
<INITIAL> nil      =>  (Tokens.NIL     (yypos,yypos+3));
<INITIAL> break    =>  (Tokens.BREAK   (yypos,yypos+5));
<INITIAL> for      =>  (Tokens.FOR     (yypos,yypos+3));
<INITIAL> of       =>  (Tokens.OF      (yypos,yypos+2));
<INITIAL> to       =>  (Tokens.TO      (yypos,yypos+2));

<INITIAL> array    =>  (Tokens.ARRAY   (yypos,yypos+5));


<INITIAL> ","	     =>  (Tokens.COMMA    (yypos,yypos+1));
<INITIAL> ":"      =>  (Tokens.COLON    (yypos,yypos+1));
<INITIAL> ";"      =>  (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL> "."      =>  (Tokens.DOT      (yypos,yypos+1));

<INITIAL> "("       =>  (Tokens.LPAREN(yypos,yypos+1));
<INITIAL> ")"       =>  (Tokens.RPAREN(yypos,yypos+1));
<INITIAL> "["       =>  (Tokens.LBRACK(yypos,yypos+1));
<INITIAL> "]"       =>  (Tokens.RBRACK(yypos,yypos+1));
<INITIAL> "{"       =>  (Tokens.LBRACE(yypos,yypos+1));
<INITIAL> "}"       =>  (Tokens.RBRACE(yypos,yypos+1));

<INITIAL> ">="     =>  (Tokens.GE    (yypos,yypos+2));
<INITIAL> ">"      =>  (Tokens.GT    (yypos,yypos+1));
<INITIAL> "<="     =>  (Tokens.LE    (yypos,yypos+2));
<INITIAL> "<"      =>  (Tokens.LT    (yypos,yypos+1));
<INITIAL> "="      =>  (Tokens.EQ    (yypos,yypos+1));
<INITIAL> "<>"     =>  (Tokens.NEQ   (yypos,yypos+2));
<INITIAL> "+"      =>  (Tokens.PLUS  (yypos,yypos+1));
<INITIAL> "-"      =>  (Tokens.MINUS (yypos,yypos+1));
<INITIAL> "*"      =>  (Tokens.TIMES (yypos,yypos+1));
<INITIAL> "/"      =>  (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL> "|"      =>  (Tokens.OR    (yypos,yypos+1));
<INITIAL> "&"      =>  (Tokens.AND   (yypos,yypos+1));
<INITIAL> ":="     =>  (Tokens.ASSIGN(yypos,yypos+2));

<INITIAL> {INT}    => (Tokens.INT(intFromString(yytext),yypos,yypos+size(yytext)));
<INITIAL> {ID}     => (Tokens.ID (yytext,yypos,yypos+size(yytext)));

<INITIAL> {COMMSTART} =>  (YYBEGIN COMMENT;  print("START\n"); updateCommentDepth(INC); continue());
<COMMENT> {COMMSTART} =>  (updateCommentDepth(INC); print("CONTINUE: " ^ yytext ^ "\n"); continue());
<COMMENT> {COMMEND}   =>  (updateCommentDepth(DEC); if !commentDepth = 0
                                                    then (print("END\n"); YYBEGIN INITIAL)
                                                    else (); 
                                                    continue());
<COMMENT> .  => (print(yytext); continue());                                                    




.       => (ErrorMsg.error yypos ("illegal character " ^ charNumberAsString(yytext)); continue());

