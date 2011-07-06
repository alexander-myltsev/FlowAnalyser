package analyser

import scala.collection.immutable._
import spsc._
import spsc.Algebra._
import Analyser._

object Analyser {
  def getEmptyMapWithTermsOrdering(): Map[Name, SortedSet[Term]] = {
    val term_ordering = Ordering.fromLessThan[Term]((x, y) => x.toString < y.toString)
    val empty_map = Map[Name, SortedSet[Term]]().withDefaultValue(SortedSet.empty(term_ordering))
    empty_map
  }

  def isGCall(t: Term) = t match {
    case GCall(_, _) => true
    case Ctr(_, _) => false
    case Name(_) => false
  }

  def isCtr(t: Term) = t match {
    case GCall(_, _) => false
    case Ctr(_, _) => true
    case Name(_) => false
  }

  def isName(t: Term) = t match {
    case GCall(_, _) => false
    case Ctr(_, _) => false
    case Name(_) => true
  }

  def matchOfCtr(pat: Pat, ctr: Ctr, tg: TreeGrammar): Option[Map[Var, Term]] = {
    if (ctr.name != pat.name) return None
    if (ctr.args.length != pat.args.length) return None
    val vars_map_list = pat.args zip ctr.args
    val vars_map = (vars_map_list :\ Map[Var, Term]()) { case ((v, t), m) => m + (v -> t) }
    Some(vars_map)
  }

  def matchAgainstPat(pat: Pat, t: Term, tg: TreeGrammar): List[Map[Var, Term]] = {
    def matchAgainstPat1(pat: Pat, currentNs: List[Name], tg: TreeGrammar, visitedNs: Set[Name]): List[Map[Var, Term]] = {
      val prods = currentNs.flatMap(n => tg.getProductions(n)).filter(!isGCall(_))
      val prods_good_ctrs =
        prods
          .filter(isCtr(_))
          .map({ case (ctr: Ctr) => matchOfCtr(pat, ctr, tg) })
          .filter(!_.isEmpty)
          .map(_.get)
      if (!prods_good_ctrs.isEmpty)
        return prods_good_ctrs

      val prods_names =
        prods
          .filter(isName(_))
          .map({ case (n: Name) => n })
          .filter(n => !visitedNs.contains(n))

      if (prods_names.isEmpty)
        return Nil

      matchAgainstPat1(pat, prods_names, tg, visitedNs ++ currentNs ++ prods_names)
    }

    t match {
      case GCall(_, _) => Nil
      case ctr @ Ctr(name, args) => matchOfCtr(pat, ctr, tg) match {
        case Some(x) => List(x)
        case None => Nil
      }
      case n @ Name(name) => matchAgainstPat1(pat, List(n), tg, Set(n))
    }
  }

  def isReachable(pat: Pat, rn: RuleName, tg: TreeGrammar): Boolean =
    matchAgainstPat(pat, rn, tg) != Nil
}

class Analyser(prog: ProgramMarked) {
  def namesOfTerm(t: Term): Set[Name] = t match {
    case GCall(_, args) => (args :\ Set[Name]()) { case (t, s) => s ++ namesOfTerm(t) }
    case Ctr(_, args) => (args :\ Set[Name]()) { case (t, s) => s ++ namesOfTerm(t) }
    case n @ Name(_) => Set(n)
  }

  def cartesianProduct(lists: List[Set[Term]]): List[List[Term]] = lists match {
    case Nil => List()
    case s :: Nil => s.toList.map(x => List(x))
    case s :: ls => s.toList.flatMap(x => cartesianProduct(ls).map(y => x :: y))
  }

  def transformGCalls(ruleName: RuleName, term: Term, treeGrammar: TreeGrammar): TreeGrammar = term match {
    case (n: Name) => TreeGrammar.create()
    case ctr @ Ctr(name, args) =>
      val temp_r = RuleName(ruleName.name + "temp")
      val r1 = args.map(t => (t, transformGCalls(temp_r, t, treeGrammar)))
      val r1_debug = r1.map(_._2)

      if (r1.forall({ case (_, tg) => tg.getProductions(temp_r).isEmpty })) {
        return TreeGrammar.create()
      }

      val r2 = r1.map({ case (t, tg) => val prods = tg.getProductions(temp_r); if (prods.isEmpty) (prods + t) else prods })
      val cp = cartesianProduct(r2)
      val ctrs = cp.map(x => Ctr(name, x))
      val treeGrammar1 = (r1 :\ TreeGrammar.create()) { case ((_, tg), tg_r) => tg_r.mergeGrammar(tg.removeRule(temp_r)) }
      val treeGrammar2 = (ctrs :\ treeGrammar1) { case (c, tg) => tg.addRule(ruleName, c) }

      //println("Ctr: " + ctr)
      //println("ctrs:\n" + ctrs.mkString("\n-----\n"))
      //println("treeGrammar1:\n" + treeGrammar1)
      //println("treeGrammar2:\n" + treeGrammar2)

      treeGrammar2
    case GCall(name, args) =>
      val matched_rs =
        prog
          .calls
          .filter(gfm => gfm.gfun.name == name)
          .filter(gfm => gfm.gfun.args.length == args.length - 1)
          .map(gfm => (gfm, matchAgainstPat(gfm.gfun.p, args.head, treeGrammar)))
          .filter(!_._2.isEmpty)
          .map({
            case (gfm, subst_pat) =>
              val subst_full = subst_pat.map(s => ((gfm.gfun.args zip args.tail) :\ s) { case ((v, t), m) => m + (v -> t) })
              val names_of_term = namesOfTerm(gfm.gfun.term)
              val subst = subst_full.map(_.filter({ case (v, t) => names_of_term.contains(v) }))
              (gfm, subst)
          })
      //println("matched_rs:\n" + matched_rs.mkString("\n"))
      val tg_rules1: List[(RuleName, RuleName)] = matched_rs.map({ case (gfm, subst) => (ruleName, gfm.ruleName) })
      val tg_rules2: List[(RuleName, Term)] = matched_rs.map({ case (gfm, subst) => (gfm.ruleName, gfm.gfun.term) })
      val tg_rules3: List[(Var, Term)] = matched_rs.flatMap(_._2).flatMap(_.toList)
      val treeGrammar1 = TreeGrammar.create().addRules(tg_rules1).addRules(tg_rules2).addRules(tg_rules3)

      // ToDo: Remove repeated code. Same code is in Ctr-case. 
      val temp_r = RuleName(ruleName.name + "temp")
      val r1 = args.map(t => (t, transformGCalls(temp_r, t, treeGrammar)))
      if (r1.forall({ case (_, tg) => tg.getProductions(temp_r).isEmpty })) {
        return treeGrammar1
      } else {
        //println(tg_rules1.mkString("\n"))
        //println(tg_rules2.mkString("\n"))
        //println(treeGrammar2)
        val r2 = r1.map({ case (t, tg) => val prods = tg.getProductions(temp_r); if (prods.isEmpty) (prods + t) else prods })
        val cp = cartesianProduct(r2)
        val gcalls = cp.map(x => GCall(name, x))
        val treeGrammar2 = (r1 :\ TreeGrammar.create()) { case ((_, tg), tg_r) => tg_r.mergeGrammar(tg.removeRule(temp_r)) }
        val treeGrammar3 = (gcalls :\ treeGrammar2) { case (gc, tg) => tg.addRule(ruleName, gc) }
        return treeGrammar3
      }
  }

  def extP(treeGrammar: TreeGrammar): TreeGrammar = {
    val treeGrammars_r = treeGrammar.listOfRRules.map({ case ((rn: RuleName), (t: Term)) => transformGCalls(rn, t, treeGrammar) })
    val treeGrammar_r = (treeGrammars_r :\ TreeGrammar.create()) { case (tg, tg_r) => tg_r.mergeGrammar(tg) }

    //println(r.mkString("\n-----\n"))
    //println(treeGrammar)
    //println(treeGrammar_r)
    //println("----------------------------")

    val treeGrammar_new = treeGrammar.mergeGrammar(treeGrammar_r)
    if (treeGrammar_new.isEqual(treeGrammar)) return treeGrammar
    else return extP(treeGrammar_new)
  }
}