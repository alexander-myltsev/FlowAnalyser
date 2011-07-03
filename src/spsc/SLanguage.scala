package spsc

abstract class Term

case class Var(varName: String) extends Name(varName) {
}

case class Ctr(name: String, args: List[Term]) extends Term {
  override def toString = name + args.mkString("(", ", ", ")")
}
case class FCall(name: String, args: List[Term]) extends Term {
  override def toString = name + args.mkString("(", ", ", ")")
}
case class GCall(name: String, args: List[Term]) extends Term {
  override def toString = name + args.mkString("(", ", ", ")")
}

case class Let(term: Term, bindings: List[(Var, Term)]) extends Term

case class Pat(name: String, args: List[Var]) {
  override def toString = name + args.mkString("(", ", ", ")")
}

abstract class Def { def name: String }
case class FFun(name: String, args: List[Var], term: Term) extends Def {
  override def toString = name + args.mkString("(", ", ", ")") + " = " + term + ";"
}
case class GFun(name: String, p: Pat, args: List[Var], term: Term) extends Def {
  override def toString = name + (p :: args).mkString("(", ", ", ")") + " = " + term + ";"
}

case class Program(defs: List[Def]) {
  val f = (defs :\ (Map[String, FFun]())) { case (x: FFun, m) => m + (x.name -> x); case (_, m) => m }
  val g = (defs :\ (Map[(String, String), GFun]())) { case (x: GFun, m) => m + ((x.name, x.p.name) -> x); case (_, m) => m }
  val gs = (defs :\ Map[String, List[GFun]]().withDefaultValue(Nil)) { case (x: GFun, m) => m + (x.name -> (x :: m(x.name))); case (_, m) => m }
  override def toString = defs.mkString("\n")
}

// -----------------------------
/*
case class VarSet(name: String) extends Term {
  override def toString = name
}

case class VarRuleDef(name: String, ctr: Ctr) {
  override def toString = name + " = " + ctr + ";"
}
*/

case class Name(name: String) extends Term {
  override def toString = name
}

case class Rule(name: Name, term: Term) {
  override def toString = name + " -> " + term + ";"
}

case class RuleName(ruleName: String) extends Name(ruleName) {
}