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

  // ---------------------------------------

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

  def drop(vs: VarSet, p: Pat, variableRulesDefs: List[VarRuleDef]): Tuple2[VarSet, List[VarRuleDef]] = {
    //println(variableRulesDefs)
    //println(vs)
    //println(p)

    val n = getNewNameOfVarRule
    val vs_r = VarSet(n)

    if (p.args.isEmpty) return (vs_r, List())

    val vrds_r = variableRulesDefs
      .filter(vrd => vrd.name == vs.name)
      //.filter(vrd => vrd.ctr.name == p.name)
      .map(vrd => vrd.ctr match {
        case ctr @ Ctr(name, List((vs1: VarSet))) if (name == p.name) =>
          Some(VarRuleDef(n, Ctr(name, List(vs_r))))
        case ctr @ Ctr(name, (c: Ctr) :: List()) =>
          if (name == p.name) Some(VarRuleDef(n, c))
          else Some(VarRuleDef(n, Ctr(name, List(c))))
        case ctr @ Ctr(name, List()) =>
          Some(VarRuleDef(n, Ctr(p.name, List(ctr))))
      })
      .map(x => x.get)
    //println(vrds_r)
    (vs_r, vrds_r)
  }

  def replace(e: Term, subst: Map[Var, Term]): Term = e match {
    case v: Var => subst.getOrElse(v, v)
    case c: Ctr => Ctr(c.name, c.args.map(x => replace(x, subst)))
    case gc: GCall => GCall(gc.name, gc.args.map(x => replace(x, subst)))
  }

  def isGCall(t: Term) = t match {
    case GCall(_, _) => true
    case v => false
  }

  //def toVarSets(terms: List[Term], vss: List[VarSet], vrds: List[VarRuleDef]): Tuple2[List[VarSet], List[VarRuleDef]] = {
  def toVarSets(terms: List[Term]): Tuple2[List[VarSet], List[VarRuleDef]] = {
    if (terms.isEmpty)
      return (List(), List())

    val (vss1, vrds1) = terms.head match {
      case c @ Ctr(name, List()) =>
        val n = getNewNameOfVarRule
        val vs = VarSet(n)
        val vrd = VarRuleDef(n, c)
        (List(vs), List(vrd))
      case Ctr(name, args) =>
        val (vss, vrds) = toVarSets(args)
        val vrds1 = vrds.map(vrd => VarRuleDef(vrd.name, Ctr(name, List(vrd.ctr))))
        (vss, vrds1)
      case (vs: VarSet) => (List(vs), List())
      case (v: Var) =>
        val vs = VarSet(getNewNameOfVarRule)
        (List(vs), List())
    }

    val (vss2, vrds2) = toVarSets(terms.tail)
    (vss1 ::: vss2, vrds1 ::: vrds2)
  }

  def eqTerms(t1: Term, t2: Term): Boolean = {
    (t1, t2) match {
      case (GCall(name1, args1), GCall(name2, args2)) =>
        name1 == name2 && ((args1 zip args2).forall(x => eqTerms(x._1, x._2)))
      case (Ctr(name1, args1), Ctr(name2, args2)) => name1 == name2
      case (v1: Var, v2: Var) => true
      case (vs1: VarSet, vs2: VarSet) => true
      case (vs: VarSet, v: Var) => true
      case (v: Var, vs: VarSet) => true
      case _ => false
    }
  }

  def analyze(rulesToProcess: Stack[Rule], variableRulesDefs: List[VarRuleDef], readyRules: List[Rule]): Tuple2[List[Rule], List[VarRuleDef]] = {
    if (rulesToProcess.isEmpty)
      return (readyRules, variableRulesDefs)

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
              val (vs1, vrds1) = drop(vs, gfun.p, variableRulesDefs)
              val (vss, vrds2) = toVarSets(rest)
              val subst_list = (gfun.p.args zip List(vs1)) ::: (gfun.args zip vss)
              val subst = (subst_list :\ Map[Var, Term]()) { case ((v, t), m) => m + (v -> t) }
              val r = Rule(getNewNameOfRule, replace(gfun.term, subst))
              (r, vrds1 ::: vrds2)
            })

        val readyRules1 = readyRules :::
          vrs.map(t2 => Rule(currentRule.name, RuleName(t2._1.name))) :::
          vrs.map(t2 => t2._1).filter(r => !isGCall(r.term))
        val rulesToProcess1_list =
          vrs
            .map(vr => vr._1)
            .filter(r => isGCall(r.term))
            .map(r => (r, (currentRule :: readyRules1).find(r1 => eqTerms(r.term, r1.term)).isEmpty))
        val rulesToProcess1 = (rulesToProcess1_list.filter(x => x._2).map(x => x._1) :\ rulesToProcess.pop) { case (v, s) => s.push(v) }
        val variableRulesDefs1 = variableRulesDefs ::: vrs.flatMap(vr => vr._2)
        val readyRules2 = readyRules1 :::
          rulesToProcess1_list
          .filter(x => !x._2)
          .map(x => {
            val r = (currentRule :: readyRules1).find(r1 => eqTerms(x._1.term, r1.term)).get
            Rule(x._1.name, RuleName(r.name))
          })

        /*
        println(rulesToProcess1)
        println(variableRulesDefs1)
        println(readyRules2)

        println()
        */

        analyze(rulesToProcess1, variableRulesDefs1, readyRules2)
    }
  }
}