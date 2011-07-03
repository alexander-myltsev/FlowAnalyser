package analyser.tests

import org.junit.Test
import org.junit.Assert._
import spsc.Algebra._
import spsc.HE._
import spsc._
import analyser._

class FlowAnalyser_Tests {
  def pP(s: String) = SParsers.parsePat(s)
  def pT(ts: String) = SParsers.parseTerm(ts)

  @Test
  def TreeGrammar_test1(): Unit = {
    val tg = TreeGrammar.create()
    assertEquals(tg.toString1(";"), "")
  }

  @Test
  def TreeGrammar_test2(): Unit = {
    val tg = TreeGrammar
      .create()
      .addRules(Var("x2"), List(pT("S(S(x2))"), pT("S(x2)"), pT("A()")))
      .addRule(Var("x1"), pT("S(S(x1))"))
      .addRule(RuleName("R1"), pT("gAdd(R1,S(Z()))"))
    //println(tg.toString1(";"))
    assertEquals(tg.toString1(";"), "R1 -> gAdd(R1, S(Z()));x1 -> S(S(x1));x2 -> A() | S(S(x2)) | S(x2)")
  }

  @Test
  def TreeGrammar_test3(): Unit = {
    val tg = SParsers.parseTreeGrammar("""
x2 -> S(S(x2));
x2 -> S(x2);
x2 -> A();
x2 -> S(x2);
x2 -> A();
R1 -> gAdd(R1,S(Z()));
x2 -> S(x2);
x2 -> A();
x1 -> S(S(x1));
R1 -> gAdd(R1,S(Z()));
""")
    //println(tg.toString1(";"))
    assertEquals(tg.toString1(";"), "R1 -> gAdd(R1, S(Z()));x1 -> S(S(x1));x2 -> A() | S(S(x2)) | S(x2)")
  }

  @Test
  def TreeGrammar_test4(): Unit = {
    val tg = TreeGrammar
      .create()
      .addRules(Var("x2"), List(pT("S(S(x2))"), pT("S(x2)"), pT("A()")))
      .addRule(Var("x1"), pT("S(S(x1))"))
      .addRule(RuleName("R1"), pT("gAdd(R1,S(Z()))"))
      .addRules(Var("a2"), List(pT("S(S(a2))"), pT("S(a2)"), pT("A()")))
      .addRule(RuleName("R2"), pT("gAdd(R1,S(Z()))"))
      .addRule(RuleName("R1"), pT("gAdd(R2,S(Z()))"))
      .addRules(Var("x1"), List(pT("Z()"), pT("S(x1)")))
      .addRule(RuleName("R1"), pT("gAdd(R1,S(Z()))"))
      .addRules(Var("x1"), List(pT("S(S(x1))"), pT("S(S(x1))"), pT("S(S(x1))"), pT("Z()")))
    //println(tg.toString1(";"))
    val tg_str = "R1 -> gAdd(R1, S(Z())) | gAdd(R2, S(Z()));R2 -> gAdd(R1, S(Z()));a2 -> A() | S(S(a2)) | S(a2);x1 -> S(S(x1)) | S(x1) | Z();x2 -> A() | S(S(x2)) | S(x2)"
    assertEquals(tg.toString1(";"), tg_str)
  }

  @Test
  def TreeGrammar_test5(): Unit = {
    val tg_str1 = """
R1 -> gAdd(x1,S(Z()));
R1 -> gAdd(R2,S(Z()));
x1 -> S(S(x1));
R2 -> y1;
y1 -> x1;
y1 -> x1;
x1 -> S(S(x1));
x1 -> S(S(x2));
x1 -> S(Z());
x1 -> S(S(x1));
y1 -> v;
"""

    val tg_str2 = """
R1 -> gAdd(x1,S(Z()));
R1 -> gAdd(x1,S(Z()));
R1 -> gAdd(x1,S(Z()));
R1 -> gAdd(x1,S(Z()));
R1 -> gAdd(R2,S(Z()));
R2 -> y1;
x1 -> S(S(x1));
x1 -> S(S(x2));
x1 -> S(Z());
x1 -> S(S(x1));
y1 -> v;
y1 -> x1;
"""

    val tg1 = SParsers.parseTreeGrammar(tg_str1)
    val tg2 = SParsers.parseTreeGrammar(tg_str2)
    assertTrue(tg1.isEqual(tg2))
    assertTrue(tg2.isEqual(tg1))
  }

  @Test
  def TreeGrammar_test6(): Unit = {
    val tgs = """
R1 -> gEven(x2);
R1 -> Cons(Cons(A(),Nil()),Nil());
R1 -> Cons(x1,x2);
R1 -> Nil();
"""
    val tg = SParsers.parseTreeGrammar(tgs)
    val tg_str = "R1 -> Cons(Cons(A(), Nil()), Nil()) | Cons(x1, x2) | Nil() | gEven(x2)"
    //println(tg.toString1(";"))
    assertEquals(tg.toString1(";"), tg_str)
  }

  @Test
  def TreeGrammar_test7(): Unit = {
    val tgs1 = """
Rtemp -> R1;
x1 -> Z();
x1 -> g;
y1 -> Z();
"""

    val tgs2 = """
Rtemp -> R1;
Rtemp -> R2;
x1 -> x;
y1 -> Z();
y2 -> Z();
"""

    val tg1 = SParsers.parseTreeGrammar(tgs1)
    val tg2 = SParsers.parseTreeGrammar(tgs2)

    assertEquals(tg1.mergeGrammar(tg2), tg2.mergeGrammar(tg1))
  }

  @Test
  def FlowAnalyzer_Match_test1(): Unit = {
    val tgs = """
x1 -> S(Z());
x1 -> S(S(x1));
"""

    val r = Analyser.matchAgainstPat(pP("S(x)"), pT("S(A(S(x1)))"), SParsers.parseTreeGrammar(tgs))
    val r_str = "Map(x -> A(S(x1)))"
    //println(r.mkString(";"))
    assertEquals(r.mkString(";"), r_str)
  }

  @Test
  def FlowAnalyzer_Match_test2(): Unit = {
    val tgs = """
x1 -> g;
g -> S(S(g));
g -> Z();
"""
    val r = Analyser.matchAgainstPat(pP("S(x)"), Var("x1"), SParsers.parseTreeGrammar(tgs))
    val r_str = "Map(x -> S(g))"
    //println(r.mkString(";"))
    assertEquals(r.mkString(";"), r_str)
  }

  @Test
  def FlowAnalyzer_Match_test3(): Unit = {
    val tgs = """
R1 -> gEven(x2);
R1 -> S(S(Z()));
R1 -> Z();
R1 -> S(x2);
R1 -> S(S(x2));
"""
    val r = Analyser.matchAgainstPat(pP("S(x)"), RuleName("R1"), SParsers.parseTreeGrammar(tgs))
    val r_str = "Map(x -> S(Z()));Map(x -> S(x2));Map(x -> x2)"
    //println(r.mkString(";"))
    assertEquals(r.mkString(";"), r_str)
  }

  @Test
  def FlowAnalyzer_Match_test4(): Unit = {
    val tgs = """
R1 -> gEven(x2);
R1 -> Cons(Cons(A(),Nil()),Nil());
R1 -> Cons(x1,x2);
R1 -> Nil();
"""
    val r = Analyser.matchAgainstPat(pP("Cons(x,y)"), RuleName("R1"), SParsers.parseTreeGrammar(tgs))
    val r_str = "Map(y -> Nil(), x -> Cons(A(), Nil()));Map(y -> x2, x -> x1)"
    //println(r.mkString(";"))
    assertEquals(r.mkString(";"), r_str)
  }

  @Test
  def FlowAnalyzer_Match_test5(): Unit = {
    val tgs = """
R1 -> gEven(x2);
R1 -> R2;
R2 -> R1;
R2 -> x2;
x2 -> g;
g -> S(g);
g -> S(Z());
"""
    val r = Analyser.matchAgainstPat(pP("S(x)"), RuleName("R1"), SParsers.parseTreeGrammar(tgs))
    val r_str = "Map(x -> Z());Map(x -> g)"
    //println(r4.mkString(";"))
    assertEquals(r.mkString(";"), r_str)
  }

  @Test
  def FlowAnalyzer_Match_test6(): Unit = {
    val tgs = """
R1 -> gEven(x2);
R1 -> S(S(Z()));
R1 -> Z();
R1 -> S(x2);
R1 -> S(S(x2));
R1 -> x2;
x2 -> S(A());
"""
    val r = Analyser.matchAgainstPat(pP("S(x)"), RuleName("R1"), SParsers.parseTreeGrammar(tgs))
    val r_str = "Map(x -> S(Z()));Map(x -> S(x2));Map(x -> x2)"
    //println(r.mkString(";"))
    assertEquals(r.mkString(";"), r_str)
  }

  @Test
  def FlowAnalyzer_Match_test7(): Unit = {
    val tgs = """
R1 -> gEven(x2);
R1 -> Cons(Cons(A(),Nil()),Nil());
R1 -> Cons(x1,x2);
R1 -> Nil();
R1 -> x2;
x2 -> Cons(x3,x4);
"""
    val r = Analyser.matchAgainstPat(pP("Cons(x,y)"), RuleName("R1"), SParsers.parseTreeGrammar(tgs))
    val r_str = "Map(y -> Nil(), x -> Cons(A(), Nil()));Map(y -> x2, x -> x1)"
    //println(r.mkString(";"))
    assertEquals(r.mkString(";"), r_str)
  }

  @Test
  def FlowAnalyzer_Match_test8(): Unit = {
    val tgs = """
R1 -> R2;
R2 -> x2;
R2 -> R1;
x2 -> Z();
"""
    val r = Analyser.matchAgainstPat(pP("S(x)"), RuleName("R1"), SParsers.parseTreeGrammar(tgs))
    //println(r.mkString(";"))
    assertEquals(r.mkString(";"), "")
  }

  @Test
  def FlowAnalyzer_Match_test9(): Unit = {
    var r = Analyser.matchAgainstPat(pP("S(x)"), pT("S(S(S(Z())))"), TreeGrammar.create())
    //println(r)
    assertEquals(r.toString, "List(Map(x -> S(S(Z()))))")
  }

  @Test
  def FlowAnalyzer_Match_test10(): Unit = {
    var r = Analyser.matchAgainstPat(pP("S(x)"), pT("A(Z())"), TreeGrammar.create())
    //println(r)
    assertEquals(r.toString, "List()")
  }

  @Test
  def FlowAnalyzer_Match_test11(): Unit = {
    val tgs = """
R0 -> gOdd(gAdd(R5, S(Z()))); 
R0 -> gOdd(gAdd(R6, S(Z()))); 
R0 -> gOdd(gAdd(gDouble(g), S(Z())));
R5 -> Z();
R6 -> S(S(gDouble(x6)));
g -> S(g)
g -> Z();
x6 -> g;
"""

    val r = Analyser.matchAgainstPat(pP("S(x)"), RuleName("R6"), SParsers.parseTreeGrammar(tgs))
    //println(r)
    assertEquals(r.toString, "List(Map(x -> S(gDouble(x6))))")
  }
}