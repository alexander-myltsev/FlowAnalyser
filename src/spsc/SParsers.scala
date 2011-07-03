package spsc

import analyser._

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.{ CharSequenceReader => Reader }

object SParsers extends StandardTokenParsers with ImplicitConversions {
  lexical.delimiters += ("(", ")", ",", "=", ";", "->")
  def prog = definition+
  def definition: Parser[Def] = gFun | fFun
  def term: Parser[Term] = fcall | gcall | ruleName | ctr | vrb
  def uid = ident ^? { case id if id.charAt(0).isUpperCase => id }
  def lid = ident ^? { case id if id.charAt(0).isLowerCase => id }
  def fid = ident ^? { case id if id.charAt(0) == 'f' => id }
  def gid = ident ^? { case id if id.charAt(0) == 'g' => id }
  def vrb = lid ^^ Var
  def pat = uid ~ ("(" ~> repsep(vrb, ",") <~ ")") ^^ Pat
  def fFun = fid ~ ("(" ~> repsep(vrb, ",") <~ ")") ~ ("=" ~> term <~ ";") ^^ FFun
  def gFun = gid ~ ("(" ~> pat) ~ ((("," ~> vrb)*) <~ ")") ~ ("=" ~> term <~ ";") ^^ GFun
  def ctr = uid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ Ctr
  def fcall = fid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ FCall
  def gcall = gid ~ ("(" ~> repsep(term, ",") <~ ")") ^^ GCall
  def parseProg(s: String) = Program(prog(new lexical.Scanner(new Reader(s))).get)
  def parseTerm(s: String) = term(new lexical.Scanner(new Reader(s))).get

  //def vid = ident ^? { case id if id.charAt(0) == 'V' => id }
  def iid = ident ^? { case id if id.charAt(0) == 'R' => id }
  def ruleName = iid ^^ RuleName
  //def varSet = vid ^^ VarSet
  //def varRuleDef = vid ~ ("=" ~> ctr <~ ";") ^^ VarRuleDef
  def rule = (ruleName | vrb) ~ ("->" ~> term <~ ";") ^^ Rule
  //def varRules = varRuleDef+
  //def parseVarRules(s: String) = varRules(new lexical.Scanner(new Reader(s))).get
  //def parseRule(s: String) = rule(new lexical.Scanner(new Reader(s))).get
  def rules = rule+
  def parseTreeGrammar(s: String) =
    TreeGrammar.createOfRules(rules(new lexical.Scanner(new Reader(s))).get)
  def parsePat(s: String) = pat(new lexical.Scanner(new Reader(s))).get
}