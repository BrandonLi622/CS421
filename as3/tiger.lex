(* CS521 Assignment 2 -- Tiger lexer 
   Gregory D. Collins
   gcollins@cs.yale.edu *)

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

val curString = ref ""
val strPosStart = ref 0
val inString = ref false;
val commentLevel = ref 0

(* For testing, reset all of our internal variables, since the test
   harness does not do this for us, and it is inconvenient to quit sml
   to test multiple files *)
(*
fun reset () = (
    ErrorMsg.reset();
    curString := "";
    inString := false;
    commentLevel := 0);
*)

fun eof () = 
let 
    val pos = hd(!linePos) 
in 
    (
     (if !inString then
          ErrorMsg.error pos ("unclosed string at end of file")
      else if !commentLevel > 0 then
          ErrorMsg.error pos ("unclosed comment at end of file")
      else ());

(*   reset(); *)
     Tokens.EOF(pos,pos))
end


(* Apologies for the lack of comments, but ML-Lex doesn't like
   them. *)

%%
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
digits=[0-9]+;
id=[a-zA-Z][a-zA-Z0-9\_]*;
ws=[\ \t\012];

%s STRING ESCAPE COMMENT ESCAPEWS;

%%

<INITIAL>{ws}+  => (continue());
<INITIAL>\n     => (lineNum := !lineNum+1; 
                    linePos := yypos :: !linePos; 
                    continue());

<INITIAL>"\""   => (YYBEGIN STRING;
                    inString := true;
                    strPosStart := yypos;
                    curString := ""; 
                    continue());

<STRING>"\""    => (YYBEGIN INITIAL;
                    inString := false;
                    Tokens.STRING((!curString),
                                  !strPosStart,
                                  yypos));

<STRING>\n      => (lineNum := !lineNum+1;
                    linePos := yypos :: !linePos;
                    ErrorMsg.error yypos ("illegal newline within string");
                    continue());

<STRING>\\      => (YYBEGIN ESCAPE; continue());

<STRING>.       => (if Char.isPrint(String.sub(yytext,0))
                        orelse Char.isSpace(String.sub(yytext,0)) then
                        curString := !curString ^ yytext
                    else
                        ErrorMsg.error yypos ("illegal character \"" 
                                              ^ yytext ^ "\" in string");
                                              
                    continue());

<ESCAPE>\n      => (YYBEGIN ESCAPEWS;
                    lineNum := !lineNum+1; 
                    linePos := yypos :: !linePos; 
                    continue());                    
<ESCAPE>{ws}    => (YYBEGIN ESCAPEWS;
                    continue());

<ESCAPEWS>\n    => (lineNum := !lineNum+1; 
                    linePos := yypos :: !linePos; 
                    continue());
<ESCAPEWS>{ws}+ => (continue());
<ESCAPEWS>"\\"  => (YYBEGIN STRING; continue());
<ESCAPEWS>.     => (ErrorMsg.error yypos ("invalid escape sequence");
                    YYBEGIN STRING; continue());

<ESCAPE>[0-9][0-9][0-9] => (
                    let
                        val c = valOf(Int.fromString(yytext))
                    in
                        (if c > Char.maxOrd then
                            ErrorMsg.error yypos 
                                ("character escape out of range: " ^ yytext)
                        else
                            curString := !curString ^ str(chr(c));

                        YYBEGIN STRING; continue())
		    end);

<ESCAPE>n       => (YYBEGIN STRING;
                    curString := !curString ^ "\n";
                    continue());
<ESCAPE>t       => (YYBEGIN STRING;
                    curString := !curString ^ "\t";
                    continue());
<ESCAPE>"\""    => (YYBEGIN STRING;
                    curString := !curString ^ "\"";
                    continue());
<ESCAPE>"\\"    => (YYBEGIN STRING;
                    curString := !curString ^ "\\";
                    continue());

<ESCAPE>\^[a-zA-Z] => (
                    let
                        val ch = Char.toUpper(String.sub(yytext,1))
                        val c = Char.ord(ch) - Char.ord(#"A") + 1
                    in
                        (
                         curString := (!curString ^ str(chr(c)));
			 YYBEGIN STRING;
                         continue())
                    end);

<ESCAPE>\^\[    => (YYBEGIN STRING;
                    curString := !curString ^ str(chr(27));
                    continue());

<ESCAPE>\^\\    => (YYBEGIN STRING;
                    curString := !curString ^ str(chr(28));
                    continue());

<ESCAPE>\^\]    => (YYBEGIN STRING;
                    curString := !curString ^ str(chr(29));
                    continue());

<ESCAPE>\^\^    => (YYBEGIN STRING;
                    curString := !curString ^ str(chr(30));
                    continue());

<ESCAPE>\^_     => (YYBEGIN STRING;
                    curString := !curString ^ str(chr(31));
                    continue());

<ESCAPE>.       => (YYBEGIN STRING;
                    ErrorMsg.error yypos ("invalid escape sequence");
                    continue());

<INITIAL>"/*"   => (YYBEGIN COMMENT; 
                    commentLevel := !commentLevel + 1;
                    continue());

<COMMENT>"/*"   => (commentLevel := !commentLevel + 1;
                    continue());

<COMMENT>"*/"   => (commentLevel := !commentLevel - 1;
                    if !commentLevel = 0 then YYBEGIN INITIAL else ();
                    continue());
<INITIAL>"*/"   => (ErrorMsg.error yypos ("unmatched '*/'");
		    continue());

<COMMENT>\n     => (lineNum := !lineNum+1; 
                    linePos := yypos :: !linePos; 
                    continue());
<COMMENT>.      => (continue());


<INITIAL>{digits} => (
                    Tokens.INT(valOf(Int.fromString yytext),
                               yypos, yypos + size yytext));


<INITIAL>while  => (Tokens.WHILE(yypos,yypos+5));
<INITIAL>for    => (Tokens.FOR(yypos,yypos+3));
<INITIAL>to     => (Tokens.TO(yypos,yypos+2));
<INITIAL>break  => (Tokens.BREAK(yypos,yypos+5));
<INITIAL>let    => (Tokens.LET(yypos,yypos+3));
<INITIAL>in     => (Tokens.IN(yypos,yypos+2));
<INITIAL>end    => (Tokens.END(yypos,yypos+3));
<INITIAL>function => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL>var    => (Tokens.VAR(yypos,yypos+3));
<INITIAL>type   => (Tokens.TYPE(yypos,yypos+4));
<INITIAL>array  => (Tokens.ARRAY(yypos,yypos+5));
<INITIAL>if     => (Tokens.IF(yypos,yypos+2));
<INITIAL>then   => (Tokens.THEN(yypos,yypos+4));
<INITIAL>else   => (Tokens.ELSE(yypos,yypos+4));
<INITIAL>do     => (Tokens.DO(yypos,yypos+2));
<INITIAL>of     => (Tokens.OF(yypos,yypos+2));
<INITIAL>nil    => (Tokens.NIL(yypos,yypos+3));

<INITIAL>":="   => (Tokens.ASSIGN(yypos,yypos+1));
<INITIAL>","    => (Tokens.COMMA(yypos,yypos+1));
<INITIAL>":"    => (Tokens.COLON(yypos,yypos+1));
<INITIAL>";"    => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>"("    => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"    => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"["    => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>"]"    => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"{"    => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"    => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"."    => (Tokens.DOT(yypos,yypos+1));
<INITIAL>"+"    => (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"-"    => (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"*"    => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"/"    => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"="    => (Tokens.EQ(yypos,yypos+1));
<INITIAL>"<>"   => (Tokens.NEQ(yypos,yypos+1));
<INITIAL>"<="   => (Tokens.LE(yypos,yypos+1));
<INITIAL>">="   => (Tokens.GE(yypos,yypos+1));
<INITIAL>"<"    => (Tokens.LT(yypos,yypos+1));
<INITIAL>">"    => (Tokens.GT(yypos,yypos+1));
<INITIAL>"&"    => (Tokens.AND(yypos,yypos+1));
<INITIAL>"|"    => (Tokens.OR(yypos,yypos+1));

<INITIAL>{id}   => (Tokens.ID(yytext,yypos,yypos+size yytext));

.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
