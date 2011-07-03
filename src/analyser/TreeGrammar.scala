package analyser

import spsc._
import scala.math.Ordering
import scala.collection.immutable._

case class TreeGrammar(rules: Map[Name, SortedSet[Term]]) {
  def toString1(delimiter: String) = {
    val sorted_rules = rules.toList.sortWith({
      case (((r1: RuleName), _), ((r2: RuleName), _)) => r1.name < r2.name
      case (((_: Var), _), ((_: RuleName), _)) => false
      case (((_: RuleName), _), ((_: Var), _)) => true
      case (((v1: Var), _), ((v2: Var), _)) => v1.name < v2.name
    })
    val sorted_rules_strings = sorted_rules.map(x => x._1 + " -> " + x._2.toList.mkString(" | "))
    sorted_rules_strings.mkString(delimiter)
  }

  override def toString = "TreeGrammar{\n" + toString1("\n") + "\n}"

  def addRule(name: Name, term: Term) = TreeGrammar.addRule(name, term, this)
  def addRules(name: Name, terms: List[Term]): TreeGrammar = TreeGrammar.addRules(name, terms, this)
  //def addRules(rules: List[(Name, List[Term])]): TreeGrammar = TreeGrammar.addRules(rules, this)
  def addRules(rules: List[(Name, Term)]): TreeGrammar = TreeGrammar.addRules(rules, this)
  def removeRule(name: Name): TreeGrammar = TreeGrammar(rules - name)
  def mergeGrammar(tg: TreeGrammar) = TreeGrammar.mergeGrammars(this, tg)
  def isEqual(tg: TreeGrammar) = TreeGrammar.areEqual(this, tg)
  def getProductions(name: Name): SortedSet[Term] = rules(name)
  val listOfRRules: List[(RuleName, Term)] =
    rules
      .filter({ case (Var(_), _) => false; case (RuleName(_), _) => true })
      .toList
      .flatMap({ case ((n: RuleName), ts) => ts.toList.map((n, _)) })
  val listOfVRules: List[(Var, Term)] =
    rules
      .filter({ case (Var(_), _) => true; case (RuleName(_), _) => false })
      .toList
      .flatMap({ case ((n: Var), ts) => ts.toList.map((n, _)) })
  /*
  val getMapOfRRules: Map[RuleName, SortedSet[Term]] =
    rules
      .filter({ case (Var(_), _) => false; case (RuleName(_), _) => true })
      .map({ case ((rn: RuleName), ts) => (rn -> ts) })     
       */
}

object TreeGrammar {
  def create(): TreeGrammar = {
    val empty_map = Analyser.getEmptyMapWithTermsOrdering()
    TreeGrammar(empty_map)
  }

  def create(rules: List[(Name, List[Term])]): TreeGrammar =
    (rules :\ TreeGrammar.create()) { case ((v, ts), tg) => TreeGrammar.addRules(v, ts, tg) }

  def addRules(name: Name, terms: List[Term], tg: TreeGrammar): TreeGrammar =
    TreeGrammar(tg.rules + (name -> (tg.rules(name) ++ terms)))

  //def addRules(rules: List[(Name, List[Term])], tg: TreeGrammar): TreeGrammar =
  //  (rules :\ tg) { case ((n, ts), tg) => tg.addRules(n, ts) }

  def addRules(rules: List[(Name, Term)], tg: TreeGrammar): TreeGrammar =
    (rules :\ tg) { case ((n, t), tg) => tg.addRule(n, t) }

  def addRule(name: Name, term: Term, tg: TreeGrammar) = addRules(name, List(term), tg)

  // ToDo: How to deal with type erasure in case "create(rules: List[Rule])"?
  def createOfRules(rules: List[Rule]): TreeGrammar =
    (rules :\ TreeGrammar.create()) { case (r, tg) => TreeGrammar.addRule(r.name, r.term, tg) }

  def areEqual(tg1: TreeGrammar, tg2: TreeGrammar) = tg1.toString == tg2.toString

  def mergeGrammars(tg1: TreeGrammar, tg2: TreeGrammar) = {
    val rules = (tg2.rules :\ tg1.rules) { case ((n, ts), rs) => rs + (n -> (rs(n) ++ ts)) }
    TreeGrammar(rules)
  }

}