/c/cs421/bin/sml <<EOF
    Control.Print.printDepth := 1000;
    CM.make "../my_sources.cm";
    (*structure R = Register;
    print "Making Semant\n";
    structure S = SemantGen(Register);
    print "Done Making Semant\n";
    S.transprog(Parse.parse("$1"));*)
    Test.comp("$1")
EOF


