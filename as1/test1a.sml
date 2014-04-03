exception Incorrect;

val prog = ASSIGN("a1", CONST 500);
if maxargs(prog) = 0 then ()
else raise Incorrect;


val prog = SEQ(ASSIGN("a2", CONST 6),PRINT[VAR"a2"]);
if maxargs(prog) = 1 then ()
else raise Incorrect;

val prog = SEQ(SEQ(ASSIGN("a3", CONST 500),ASSIGN("b", BINOP(VAR"a3", MINUS, ESEQ(PRINT[VAR "a3"], CONST 100)))),
	       PRINT [ESEQ(PRINT[VAR"b"],VAR "a3"), 
		      BINOP(VAR "a3", TIMES, VAR "a3"), 
		      BINOP(ESEQ(ASSIGN("a3div2", BINOP(VAR"a3", DIV , CONST 2)), 
				 ESEQ(ASSIGN("a3div2plusa3", BINOP(ESEQ(ASSIGN("a3diva3",
									       BINOP(VAR"a3", DIV,VAR"a3")
									      ),
									ESEQ(PRINT[VAR"a3diva3"],VAR"a3div2")
								   ),
						   	           PLUS, 
							           VAR"a3")
					    ), 
				      VAR"a3div2plusa3"
				     )
			        ), 
			    MINUS, 
			    VAR"a3"
		           )
		     ]
	      ) ;
if maxargs(prog) = 3 then ()
else raise Incorrect;

val prog = PRINT[CONST(10),
                 ESEQ(PRINT[CONST(11)], 
                      CONST(12)),
                 CONST(13)];
if maxargs(prog) = 3 then ()
else raise Incorrect;

val prog = SEQ(ASSIGN("a", BINOP(CONST 3, TIMES, CONST 2)),
                SEQ(ASSIGN("b", BINOP(VAR"a", DIV, CONST 3)),
                    PRINT[VAR"a", VAR "b", BINOP(ESEQ(ASSIGN("b", CONST 1000), VAR "a"), TIMES, VAR "b")]
                )
           );
if maxargs(prog) = 3 then ()
else raise Incorrect;

val prog = SEQ(ASSIGN("a",BINOP(CONST 5, PLUS, CONST 3)),
               SEQ(ASSIGN("b",ESEQ(PRINT[VAR"a",VAR"a",BINOP(ESEQ(PRINT[VAR"a",VAR"a",VAR"a",VAR"a"],VAR"a"),MINUS,CONST 1)],  
                                   BINOP(CONST 10, TIMES, VAR"a")
                                  )
                         ),
                   PRINT[VAR"b",VAR"b",VAR"b",VAR"b",VAR"b",VAR"a"]
                  )
              );
if maxargs(prog) = 6 then ()
else raise Incorrect;

val prog = SEQ(ASSIGN("a",BINOP(CONST 5, PLUS, CONST 3)),
               SEQ(ASSIGN("b",ESEQ(PRINT[VAR"a",BINOP(VAR"a",MINUS,ESEQ(PRINT[VAR "a", VAR "a", VAR "a", VAR "a", VAR "a"],CONST 1))],  
                                   BINOP(CONST 10, TIMES, VAR"a")
                                  )
                         ),
                   PRINT[VAR"b"]
                  )
              );
if maxargs(prog) = 5 then ()
else raise Incorrect;


val prog = PRINT[];
if maxargs(prog) = 0 then ()
else raise Incorrect;


val prog = ASSIGN("a3", CONST 500);
if maxargs(prog) = 0 then ()
else raise Incorrect;


val prog = SEQ(ASSIGN("a3", CONST 500),
               PRINT[VAR("a3")]);
if maxargs(prog) = 1 then ()
else raise Incorrect;


val prog = PRINT[ESEQ(PRINT[CONST(10), 
                            CONST(11)], 
                      CONST(10))];
if maxargs(prog) = 2 then ()
else raise Incorrect;

val prog = SEQ(ASSIGN("a3", CONST 500),
               PRINT[VAR("a3"),
                     CONST(10),
                     BINOP(VAR("a3"), DIV, CONST(10)),
                     ESEQ(ASSIGN("a3", CONST 400),
                          VAR("a3"))]);
if maxargs(prog) = 4 then ()
else raise Incorrect;

val prog = SEQ(SEQ(ASSIGN("a3", CONST 500), 
                   ASSIGN("a3", CONST 400)), 
               PRINT[VAR("a3")]);
if maxargs(prog) = 1 then ()
else raise Incorrect;


val prog = PRINT[ESEQ(ASSIGN("a3", CONST 500), 
                      VAR("a3")), 
                 ESEQ(ASSIGN("a3", CONST 400), 
                      VAR("a3")), 
                 ESEQ(PRINT[ESEQ(ASSIGN("a3", CONST 300), 
                                 VAR("a3"))], 
                      VAR("a3"))];
if maxargs(prog) = 3 then ()
else raise Incorrect;

val prog = SEQ(ASSIGN("a3", CONST 500), 
               PRINT[ESEQ(PRINT[ESEQ(PRINT[VAR("a3"), 
                                           CONST ~10, 
                                           BINOP(VAR"a3", DIV, VAR"a3")], 
                                     CONST 11)], 
                          CONST 10)]);
if maxargs(prog) = 3 then ()
else raise Incorrect;
