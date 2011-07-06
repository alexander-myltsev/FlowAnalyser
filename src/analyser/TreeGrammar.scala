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
  def create(): TreeGrammar = TreeGrammar(Analyser.getEmptyMapWithTermsOrdering())

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

  object Cleaner {
    private def hasGCall(term: Term): Boolean = term match {
      case Ctr(_, args) => !args.find(t => hasGCall(t)).isEmpty
      case Name(_) => false
      case GCall(_, _) => true
    }

    /*
    def eraseGFuns(tg: TreeGrammar): TreeGrammar = {
      val rules = tg.rules.map({ case (n, ts) => (n -> (ts.filter(x => !hasGCall(x)))) })
      TreeGrammar(rules)
    }
    */

    def eraseLoops(tg: TreeGrammar): TreeGrammar = {
      // http://en.wikipedia.org/wiki/Loop_%28graph_theory%29
      val rules =
        tg
          .rules
          .map({ case (n, ts) => (n -> (ts.filter(x => n != x).toList)) })
          .toList
      TreeGrammar.create(rules)
    }

    def eraseTransitiveRules(tg: TreeGrammar): TreeGrammar = {
      def replaceTerm(inputTerm: Term, nameToReplaceFrom: Name, nameToReplaceTo: Name): Term = {
        inputTerm match {
          case GCall(name, args) => GCall(name, args.map(arg => replaceTerm(arg, nameToReplaceFrom, nameToReplaceTo)))
          case Ctr(name, args) => Ctr(name, args.map(arg => replaceTerm(arg, nameToReplaceFrom, nameToReplaceTo)))
          case r @ Name(name) => if (name == nameToReplaceFrom.name) nameToReplaceTo else r
        }
      }

      //println(tg)

      val rule_for_remove = tg.rules.find({ case (r, ts) => ts.size == 1 && ts.head.isInstanceOf[Name] && r.name != "R0" })
      rule_for_remove match {
        case None => tg
        case Some((rule_to_remove, set_of_terms)) =>
          val rule_substitute = tg.rules(rule_to_remove).head.asInstanceOf[Name]
          val rules =
            tg
              .removeRule(rule_to_remove)
              .rules.map({
                case (rn, ts) =>
                  //val ts_new = ts.empty ++ ts.map(x => replaceTerm(x, rule_to_remove, rule_substitute))
                  val ts_new = ts.map(x => replaceTerm(x, rule_to_remove, rule_substitute)).toList
                  (rn, ts_new)
              })
              .toList
          val tg_new = eraseLoops(TreeGrammar.create(rules))
          eraseTransitiveRules(tg_new)
      }
    }

    def clean(tg: TreeGrammar): TreeGrammar = {
      eraseTransitiveRules(tg)
    }
  }
}