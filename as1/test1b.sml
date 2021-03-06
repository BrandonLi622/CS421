exception Incorrect;

(* "" *)
val prog = ASSIGN("a1", CONST 500);
interp(prog);

(* "6\n" *)
val prog = SEQ(ASSIGN("a2", CONST 6),PRINT[VAR"a2"]);
interp(prog);

(* "500\n400\n500 250000 1\n250\n" *)
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
interp(prog);

(* 10 11\n12 13\n *)
val prog = PRINT[CONST(10),
                 ESEQ(PRINT[CONST(11)], 
                      CONST(12)),
                 CONST(13)];
interp(prog);

(* 6 2 6000\n *)
val prog = SEQ(ASSIGN("a", BINOP(CONST 3, TIMES, CONST 2)),
                SEQ(ASSIGN("b", BINOP(VAR"a", DIV, CONST 3)),
                    PRINT[VAR"a", VAR "b", BINOP(ESEQ(ASSIGN("b", CONST 1000), VAR "a"), TIMES, VAR "b")]
                )
           );
interp(prog);

(* 8 8 8 8 8 8\n7\n80 80 80 80 80 8\n *)
val prog = SEQ(ASSIGN("a",BINOP(CONST 5, PLUS, CONST 3)),
               SEQ(ASSIGN("b",ESEQ(PRINT[VAR"a",VAR"a",BINOP(ESEQ(PRINT[VAR"a",VAR"a",VAR"a",VAR"a"],VAR"a"),MINUS,CONST 1)],  
                                   BINOP(CONST 10, TIMES, VAR"a")
                                  )
                         ),
                   PRINT[VAR"b",VAR"b",VAR"b",VAR"b",VAR"b",VAR"a"]
                  )
              );
interp(prog);

(* 8 8 8 8 8 8\n7\n80\n *)
val prog = SEQ(ASSIGN("a",BINOP(CONST 5, PLUS, CONST 3)),
               SEQ(ASSIGN("b",ESEQ(PRINT[VAR"a",BINOP(VAR"a",MINUS,ESEQ(PRINT[VAR "a", VAR "a", VAR "a", VAR "a", VAR "a"],CONST 1))],  
                                   BINOP(CONST 10, TIMES, VAR"a")
                                  )
                         ),
                   PRINT[VAR"b"]
                  )
              );
interp(prog);

(* "\n" *)
val prog = PRINT[];
interp(prog);

(* "" *)
val prog = ASSIGN("a3", CONST 500);
interp(prog);

(* "500\n" *)
val prog = SEQ(ASSIGN("a3", CONST 500),
               PRINT[VAR("a3")]);
interp(prog);

(* "10 11\n10" *)
val prog = PRINT[ESEQ(PRINT[CONST(10), 
                            CONST(11)], 
                      CONST(10))];
interp(prog);

(* "500 10 50 400\n" *)
val prog = SEQ(ASSIGN("a3", CONST 500),
               PRINT[VAR("a3"),
                     CONST(10),
                     BINOP(VAR("a3"), DIV, CONST(10)),
                     ESEQ(ASSIGN("a3", CONST 400),
                          VAR("a3"))]);
interp(prog);

(* "400\n" *)
val prog = SEQ(SEQ(ASSIGN("a3", CONST 500), 
                   ASSIGN("a3", CONST 400)), 
               PRINT[VAR("a3")]);
interp(prog);

(* "500 400 300\n300\n" *)
val prog = PRINT[ESEQ(ASSIGN("a3", CONST 500), 
                      VAR("a3")), 
                 ESEQ(ASSIGN("a3", CONST 400), 
                      VAR("a3")), 
                 ESEQ(PRINT[ESEQ(ASSIGN("a3", CONST 300), 
                                 VAR("a3"))], 
                      VAR("a3"))];
interp(prog);

(* "500 -10 1\n11\n10\n" *)
val prog = SEQ(ASSIGN("a3", CONST 500), 
               PRINT[ESEQ(PRINT[ESEQ(PRINT[VAR("a3"), 
                                           CONST ~10, 
                                           BINOP(VAR"a3", DIV, VAR"a3")], 
                                     CONST 11)], 
                          CONST 10)]);
interp(prog);

val prog = SEQ(SEQ(PRINT([CONST(3)]), ASSIGN("a", ESEQ(PRINT([CONST(2)]), CONST(35)))), 
               SEQ(ASSIGN("b", BINOP(VAR("a"), PLUS, CONST(50))), PRINT([VAR("a"), VAR("b")])));
interp(prog);
