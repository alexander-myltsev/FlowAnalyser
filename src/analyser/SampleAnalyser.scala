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
    gEven(S(x1)) = gOdd(x1);

    gOdd(Z()) = False();
    gOdd(S(x1)) = gEven(x1);

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
  val inputRule = """
R0 = gAdd(V0, Z());
"""
  val varRules = """
V0 = S(S(Z()));
V0 = S(V0);
"""
  val program = """
gAdd(S(x), y) = gAdd(x, S(y));
gAdd(Z(), y) = y;
"""

  def main(args: Array[String]): Unit = {
    run(inputRule, varRules, program)
  }

  def run(inputRuleText: String, variableRulesText: String, programText: String): Unit = {
    val program = SParsers.parseProg(programText)
    val inputRule = SParsers.parseRule(inputRuleText)
    val variableRules = SParsers.parseVarRules(variableRulesText)
    val analyser = new Analyser(program)
    val (rules, varRules) = analyser.analyze(immutable.Stack(inputRule), variableRules, List[Rule]())
    println("------------- result -------------")
    println("List of rules:\n" + (rules :\ "") { case (r, s) => r + "\n" + s })
    println("----------------------------------")
    println("List of var rules:\n" + (varRules :\ "") { case (r, s) => r + "\n" + s })
    println()
  }

  def runInterpreter(targetText: String, programText: String) = {
    val program = SParsers.parseProg(programText)
    val target = SParsers.parseTerm(targetText)
    val analyser = new Analyser(program)
    val pt = analyser.interpret(target)
    println(pt)
  }
}