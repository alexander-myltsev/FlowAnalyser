package analyser

import scala.collection.immutable._
import spsc._
import spsc.Algebra._

/*
case class Domain(start: Ctr, prod: Ctr) extends Term {

}
*/

class Analyser(prog: Program) {
  def interpret(e: Term): Term = e match {
    case c: Ctr => c
    //case v: Var => v
    case gcall @ GCall(name, (ctr: Ctr) :: rest) =>
      val gfuns = prog.gs(name)
      val gfun = gfuns.find(_.p.name == ctr.name).get
      val subst_list = (gfun.p.args zip ctr.args) ::: (gfun.args zip rest)
      val subst = (subst_list :\ Map[Var, Term]()) { case ((v, t), m) => m + (v -> t) }

      def replace(e: Term): Term = e match {
        case v: Var => subst.getOrElse(v, v)
        case c: Ctr => Ctr(c.name, c.args.map(replace(_)))
        case gc: GCall => GCall(gc.name, gc.args.map(replace(_)))
      }

      val gc = interpret(replace(gfun.term))
      //println(gc)
      gc
    case GCall(name, (arg: GCall) :: rest) =>
      val r = interpret(arg)
      val gc = interpret(GCall(name, r :: rest))
      //println(gc)
      gc
  }

  var indexOfRule = 0
  def getNewNameOfRule: String = { indexOfRule += 1; "R" + indexOfRule }

  var indexOfVarRule = 0
  def getNewNameOfVarRule: String = { indexOfVarRule += 1; "V" + indexOfVarRule }

  def makeVarRuleDef(gfun: GFun, variableRulesDefs: List[VarRuleDef]): VarRuleDef = {
    val name = getNewNameOfVarRule
    val vrd = variableRulesDefs.filter({ case vrd => vrd.name == gfun.p.name })
    VarRuleDef(name, null)
  }

  def isMatched(pat: Pat, vs: VarSet, variableRulesDefs: List[VarRuleDef]) = {
    //println(variableRulesDefs)
    //println(pat)
    //println(vs)

    val isNotMatched =
      variableRulesDefs
        .filter(vrd => vrd.name == vs.name)
        .map(vrd => vrd.ctr)
        .find({ case ctr => ctr.name == pat.name })
        .isEmpty
    !isNotMatched
  }

  def drop(vs: VarSet, p: Pat, variableRulesDefs: List[VarRuleDef]): Tuple2[VarSet, Option[VarRuleDef]] = {
    //println(variableRulesDefs)
    //println(vs)
    //println(p)

    val vrds = variableRulesDefs
      .filter(vrd => vrd.name == vs.name)
      .filter(vrd => vrd.ctr.name == p.name)
      .map(vrd => vrd.ctr match {
        case ctr @ Ctr(name, List((vs1: VarSet))) =>
          val n = getNewNameOfVarRule
          val vs_r = VarSet(n)
          (vs_r, Some(VarRuleDef(n, Ctr(name, List(vs_r)))))
        case ctr @ Ctr(name, (c: Ctr)) =>
          val n = getNewNameOfVarRule
          val vs_r = VarSet(n)
          (vs_r, Some(VarRuleDef(n, c)))
        case Ctr(name, List()) =>
          (VarSet(getNewNameOfVarRule), None)
      })
    //println(vrds)
    assert(vrds.length == 1) // fix it. vrds.len could be > 1
    vrds.head
  }

  def replace(e: Term, subst: Map[Var, Term]): Term = e match {
    case v: Var => subst.getOrElse(v, v)
    case c: Ctr => Ctr(c.name, c.args.map(x => replace(x, subst)))
    case gc: GCall => GCall(gc.name, gc.args.map(x => replace(x, subst)))
  }

  def analyze(rulesToProcess: Stack[Rule], variableRulesDefs: List[VarRuleDef], readyRules: List[Rule]): List[Rule] = {
    val currentRule = rulesToProcess.top
    currentRule.term match {
      case GCall(name, (vs: VarSet) :: rest) =>
        val vrs =
          prog
            .gs(name)
            .filter(gfun => isMatched(gfun.p, vs, variableRulesDefs))
            .map(gfun => {
              //println(gfun)
              //println(currentRule)
              val (vs1, vrd1) = drop(vs, gfun.p, variableRulesDefs)
              val subst_list = (gfun.p.args zip List(vs1)) ::: (gfun.args zip rest)
              val subst = (subst_list :\ Map[Var, Term]()) { case ((v, t), m) => m + (v -> t) }
              vrd1 match {
                case None =>
                  val r = Rule(getNewNameOfRule, replace(gfun.term, subst))
                  (r, None)
                case Some(x) =>
                  val r = Rule(getNewNameOfRule, replace(gfun.term, subst))
                  (r, vrd1)
              }
            })

        val rulesToProcess1: Stack[Rule] = (vrs.map(vr => vr._1) :\ rulesToProcess.pop) { case (v, s) => s.push(v) }
        val variableRulesDefs1: List[VarRuleDef] = vrs.filter(vr => !vr._2.isEmpty).map(vr => vr._2.get)
        val readyRules1: List[Rule] = vrs.map(vr => Rule(currentRule.name, VarSet(vr._1.name)))

        println(rulesToProcess1)
        println(variableRulesDefs1)
        println(readyRules1)

        println()

        analyze(rulesToProcess1, variableRulesDefs ::: variableRulesDefs1, readyRules1)
    }
    variableRulesDefs
  }
}