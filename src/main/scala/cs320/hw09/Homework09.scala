package cs320

import scala.util.parsing.combinator._

trait Homework09 extends Homework with RegexParsers  {
  // Abstract syntax
  trait COREL
  case class Num(n: Int) extends COREL
  case class Bool(b: Boolean) extends COREL
  case class Add(lhs: COREL, rhs: COREL) extends COREL
  case class Sub(lhs: COREL, rhs: COREL) extends COREL
  case class Equ(lhs: COREL, rhs: COREL) extends COREL
  case class With(name: String, ty: Type, expr: COREL, body: COREL) extends COREL
  case class Id(name: String) extends COREL
  case class Fun(param: String, paramty: Type, body: COREL) extends COREL
  case class App(funE: COREL, argE: COREL) extends COREL
  case class IfThenElse(
    testE: COREL,
    thenE: COREL, elseE: COREL
  ) extends COREL
  case class Rec(fname: String, fty: Type,
                 pname: String, pty: Type,
                 body: COREL) extends COREL
  case class WithType(
    name: String,
    constructors: Map[String, Type],
    body: COREL
  ) extends COREL
  case class Cases(
    name: String,
    dispatchE: COREL,
    cases: Map[String, (String, COREL)]
  ) extends COREL

  // Parser for COREL
  object COREL {
    def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
    lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)
    lazy val str: Parser[String] = """[a-zA-Z][a-zA-Z0-9_-]*""".r
    lazy val pair: Parser[(String, Type)] = wrap(str ~ ty) ^^ { case s ~ t => (s, t) }
    lazy val triple: Parser[(String, String, COREL)] =
      wrap(str ~ wrap(str) ~ expr) ^^ { case c ~ a ~ e => (c, a, e) }
    lazy val ty: Parser[Type] = Type.ty
    lazy val expr: Parser[COREL] =
      int                                           ^^ { case n => Num(n) }                                 |
      "true"                                        ^^ { case _ => Bool(true) }                             |
      "false"                                       ^^ { case _ => Bool(false) }                            |
      wrap("+" ~> expr ~ expr)                      ^^ { case l ~ r => Add(l, r) }                          |
      wrap("-" ~> expr ~ expr)                      ^^ { case l ~ r => Sub(l, r) }                          |
      wrap("=" ~> expr ~ expr)                      ^^ { case l ~ r => Equ(l, r) }                          |
      str                                           ^^ { case x => Id(x) }                                  |
      wrap("with" ~> wrap(
        (str <~ ":") ~ ty ~ expr
      ) ~ expr)                                     ^^ { case x ~ t ~ i ~ b => With(x, t, i, b) }           |
      wrap("fun" ~> wrap((str <~ ":") ~ ty) ~ expr) ^^ { case p ~ t ~ b => Fun(p, t, b) }                   |
      wrap("if" ~> expr ~ expr ~ expr)              ^^ { case c ~ t ~ e => IfThenElse(c, t, e) }            |
      wrap("recfun" ~> wrap(
        (str <~ ":") ~ ty ~ (str <~ ":") ~ ty
      ) ~ expr)                                     ^^ { case f ~ ft ~ x ~ xt ~ b => Rec(f, ft, x, xt, b) } |
      wrap("withtype" ~> wrap(
        str ~
        rep1(pair)
      ) ~ expr) ^^ {
        case (n ~ cs) ~ e =>
          val m = cs.map { case (x, t) => x -> t }.toMap
          WithType(n, m, e)
      }                                                                                          |
      wrap("cases" ~>
        str ~
        expr ~
        rep1(triple)
      ) ^^ {
        case n ~ e ~ cs =>
          val l = cs.map { case (x, v, e) => x -> (v, e) }.toMap
          Cases(n, e, l)
      }                                                                                          |
      wrap(expr ~ expr)                          ^^ { case f ~ a => App(f, a) }
    def apply(str: String): COREL = parseAll(expr, str).getOrElse(error(s"bad syntax: $str"))
  }

  // Type
  trait Type
  case object NumT extends Type
  case object BoolT extends Type
  case class ArrowT(param: Type, result: Type) extends Type
  case class IdT(name: String) extends Type

  // Parser for Type
  object Type {
    def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
    lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)
    lazy val str: Parser[String] = """[a-zA-Z][a-zA-Z0-9_-]*""".r
    lazy val ty: Parser[Type] =
      str                     ^^ { case "num"  => NumT
                                   case "bool" => BoolT
                                   case x      => IdT(x)
                                 }                                  |
      wrap(ty ~ "->" ~ ty)    ^^ { case p ~ _ ~ r => ArrowT(p, r) }
    def apply(str: String): Type = parse(ty, str).getOrElse(error(s"bad syntax: $str"))
  }

  // Check the type of a given expression contained in a string
  def typeCheck(str: String): Type

  // Evaluate an expression contained in a string
  def interp(str: String): String

  // Type-check and interpretation of a given expression contained in a string
  def run(str: String): String = { typeCheck(str); interp(str) }

  // Write your own tests
  def tests: Unit
}
