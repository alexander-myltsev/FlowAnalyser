package analyser

import spsc._
import scala.collection.immutable

object SamplesForInterpreter {
  val target0 = "gApp(Cons(Cons(Nil(), Nil()), Nil()))"
  val program0 =
    """
    gApp(Nil(), vs) = vs; 
    gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));
    """

  //val target1 = "gAdd(S(S(Z())), S(Z()))"
  //val target1 = "gAdd(S(S(Z())), Z())"
  //val target1 = "gAdd(Z(), S(S(S(Z()))))"
  val target1 = "gAdd(Z(), Z())"
  val program1 =
    """
    gAdd(Z(), y) = y; 
    gAdd(S(x), y) = gAdd(x, S(y));
    """

  val target2 = "gEven(S(S(gDouble(S(S(S(Z())))))))"
  val program2 =
    """
    gEven(Z()) = True();
    gEven(S(x)) = gOdd(x);

    gOdd(Z()) = False();
    gOdd(S(x)) = gEven(x);

    gDouble(Z()) = Z();
    gDouble(S(x)) = S(S(gDouble(x)));

    gFun(n) = gEven(gDouble(n));
    """

  val target3 = "gMult(S(S(Z())), S(S(S(Z()))))"
  val program3 =
    """
	gMult(S(x), y) = gAdd(gMult(x, y), y);
    gMult(Z(), y) = Z();

    gAdd(S(x), y) = gAdd(x, S(y));
    gAdd(Z(), y) = y;
    """

  //val target4 = "gEq(S(S(S(S(Z())))), S(S(S(S(Z())))))"
  //val target4 = "gEq(S(S(S(Z()))), S(S(S(S(Z())))))"
  //val target4 = "gEq(S(S(S(Z()))), S(S(Z())))"
  val target4 = "gEq(S(Z()), S(Z()))"
  //val target4 = "gEq(Z(), S(Z()))"
  //val target4 = "gEq(S(Z()), Z())"
  //val target4 = "gEq(Z(), Z())"
  val program4 =
    """
    gEq(Z(), y) = gEqz(y);
    gEq(S(x), y) = gEqs(y, x);
    gEqz(Z()) = True();
    gEqz(S(x)) = False();
    gEqs(Z(), x) = False();
    gEqs(S(y), x) = gEq(x, y);
    """
}

object SampleAnalyser {

  val program0 = """
gAdd(S(x), y) = gAdd(x, S(y));
gAdd(Z(), y) = y;
"""
  val treeGrammar0_0 = """
R0 -> gAdd(S(Z()), Z());
"""
  val treeGrammar0_1 = """
R0 -> gAdd(g, S(Z()));
g -> S(g);
g -> S(Z());
"""
  val treeGrammar0_2 = """
R0 -> gAdd(gAdd(g, Z()), gAdd(x, S(Z())));
x -> S(x);
x -> Z();
g -> S(g);
g -> Z();
"""
  val treeGrammar0_3 = """
R0 -> S(gAdd(g, Z()));
g -> S(g);
g -> S(Z());
"""
  // ------------------------------------
  val program1 = """
gAdd(S(x)) = S(gAdd(x));
gAdd(Z()) = Z();
"""
  val treeGrammar1_0 = """
R0 -> gAdd(g);
g -> S(S(g));
g -> Z();
"""
  // ------------------------------------
  val program2 = """
gAdd(Z(), y) = y;
gAdd(S(x), y) = S(gAdd(x, y));
gMult(Z(), y) = Z();
gMult(S(x), y) = gAdd(gMult(x, y), y); 
"""
  val treeGrammar2_0 = """
R0 -> gMult(Z(), g);
g -> S(S(g));
g -> Z();
"""
  val treeGrammar2_1 = """
R0 -> gMult(S(Z()), g);
g -> S(g);
g -> Z();
"""
  val treeGrammar2_2 = """
R0 -> gMult(S(Z()), g);
g -> S(S(g));
g -> S(Z());
"""
  val treeGrammar2_3 = """
R0 -> gMult(g, S(S(Z())));
g -> S(g);
g -> Z();
"""
  // ------------------------------------
  val program3 = """
gAppend(Nil(), vs) = vs;
gAppend(Cons(u, us), vs) = Cons(u, gAppend(us, vs));
"""
  val treeGrammar3_0 = """
R0 -> gAppend(Cons(A(), Cons(B(), Cons(C(), Nil()))), Cons(D(), Cons(E(), Nil()))); 
"""
  val treeGrammar3_1 = """
R0 -> gAppend(Cons(A(), Nil()), Nil());
"""
  val treeGrammar3_2 = """
R0 -> gAppend(Nil(), g);
g -> Cons(A(), g);
g -> Nil();
"""
  val treeGrammar3_3 = """
R0 -> gAppend(g, Nil());
g -> Cons(A(), g);
g -> Nil();
"""
  // ------------------------------------
  val program4 = """
gEven(Z()) = True();
gEven(S(x)) = gOdd(x);
gOdd(Z()) = False();
gOdd(S(x)) = gEven(x);
gDouble(Z()) = Z();
gDouble(S(x)) = S(S(gDouble(x)));
"""
  val treeGrammar4_0 = """
R0 -> gEven(S(S(S(Z()))));
"""
  val treeGrammar4_1 = """
R0 -> gOdd(S(S(S(Z()))));
"""
  val treeGrammar4_2 = """
R0 -> gEven(g);
g -> S(S(g));
g -> Z();
"""
  val treeGrammar4_3 = """
R0 -> gEven(g);
g -> S(S(g));
g -> S(Z());
"""
  val treeGrammar4_4 = """
R0 -> gOdd(g);
g -> S(S(g));
g -> S(Z());
"""
  val treeGrammar4_5 = """
R0 -> gEven(gDouble(g));
g -> S(g);
g -> Z();
"""
  // ------------------------------------
  val program5 = """
gEven(Z()) = True();
gEven(S(x)) = gOdd(x);
gOdd(Z()) = False();
gOdd(S(x)) = gEven(x);
gDouble(Z()) = Z();
gDouble(S(x)) = S(S(gDouble(x)));
gAdd(S(x), y) = gAdd(x, S(y));
gAdd(Z(), y) = y;
"""
  val treeGrammar5_0 = """
R0 -> gOdd(gAdd(gDouble(g), S(Z())));
g -> S(g);
g -> Z();
"""
  val treeGrammar5_1 = """
R0 -> gEven(gAdd(gDouble(g), S(S(Z()))));
g -> S(g);
g -> Z();
"""
  val treeGrammar5_2 = """
R0 -> gEven(gAdd(gAdd(gDouble(g), S(Z())), S(Z())));
g -> S(g);
g -> Z();
"""
  val treeGrammar5_3 = """
R0 -> gEven(gDouble(g));
g -> S(g);
g -> Z();
"""

  def main(args: Array[String]): Unit = {
    //run(program0, treeGrammar0_1)
    //run(program0, treeGrammar0_2)
    //run(program0, treeGrammar0_3)
    //run(program1, treeGrammar1_0)
    //run(program2, treeGrammar2_0)
    //run(program2, treeGrammar2_1)
    //run(program2, treeGrammar2_2)
    //run(program2, treeGrammar2_3)
    //run(program3, treeGrammar3_0)
    //run(program3, treeGrammar3_1)
    //run(program3, treeGrammar3_2)
    //run(program3, treeGrammar3_3)
    //run(program4, treeGrammar4_0)
    //run(program4, treeGrammar4_1)
    //run(program4, treeGrammar4_3)
    //run(program4, treeGrammar4_4)
    //run(program4, treeGrammar4_5)
    //run(program5, treeGrammar5_0)
    //run(program5, treeGrammar5_1)
    //run(program5, treeGrammar5_2)
    //run(program5, treeGrammar5_3)
    
    reachability()
  }

  def reachability() = {
    val tgs = """
R0 -> R1;
R0 -> R2;
R1 -> R2;
R2 -> x2;
x2 -> True();
"""

    def pP(s: String) = SParsers.parsePat(s)

    val tg = TreeGrammar.Cleaner.eraseTransitiveRules(SParsers.parseTreeGrammar(tgs))
    //val tg = SParsers.parseTreeGrammar(tgs)
    val f1 = Analyser.isReachable(pP("False()"), RuleName("R0"), tg)
    val f2 = Analyser.isReachable(pP("True()"), RuleName("R0"), tg)
    
    println(f1)
    println(f2)
  }

  def run(programText: String, treeGrammarText: String) = {
    val program = SParsers.parseProg(programText)
    val markedProgram = new ProgramMarked(program)
    val initialTreeGrammar = SParsers.parseTreeGrammar(treeGrammarText)
    val approximatedTreeGrammar = (new Analyser(markedProgram)).extP(initialTreeGrammar)
    println("======================")
    println(markedProgram)
    println("----------------------")
    println("Initial " + initialTreeGrammar)
    println("----------------------")
    //println("Approximated " + approximatedTreeGrammar)
    println("Approximated " + TreeGrammar.Cleaner.clean(approximatedTreeGrammar))
    println("======================")
  }
}