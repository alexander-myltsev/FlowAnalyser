package analyser

import spsc._

case class GFunMarked(index: Int, gfun: GFun) {
  val ruleName = RuleName("R" + index)
  override def toString = ruleName + ": " + gfun
}

class ProgramMarked(prog: Program) {
  def transformTerm(term: Term, i: Int): Term = term match {
    case Var(name) => Var(name + i)
    case Ctr(name, args) => Ctr(name, args.map(x => transformTerm(x, i)))
    case GCall(name, args) => GCall(name, args.map(x => transformTerm(x, i)))
  }

  def transform(gfun: GFun, i: Int): GFun = {
    val pat = Pat(gfun.p.name, gfun.p.args.map(v => transformTerm(v, i).asInstanceOf[Var]))
    val args = gfun.args.map(v => transformTerm(v, i).asInstanceOf[Var])
    val term = transformTerm(gfun.term, i)
    GFun(gfun.name, pat, args, term)
  }

  val calls = {
    val n = prog.defs.length
    val marked_gfuns = (prog.defs :\ (n, List[GFunMarked]())) {
      case ((x: GFun), (i, xs)) => (i - 1, GFunMarked(i, transform(x, i)) :: xs)
      case ((x: FFun), (i, xs)) => (i, xs)
    }
    marked_gfuns._2
  }

  override def toString = "ProgramMarked{\n" + calls.mkString("\n") + "\n}"
}