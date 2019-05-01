package cs320

package object hw03 extends Homework03 {

  trait FWAEValue

  case class NumV(n: Int) extends FWAEValue

  case class CloV(params: List[String], body: MRFWAE, env: Map[String, FWAEValue]) extends FWAEValue

  case class RecV(records: Map[String, FWAEValue]) extends FWAEValue

  val argList: Map[String, FWAEValue] = Map.empty[String, FWAEValue]

  def run(str: String): String = interpret(MRFWAE.apply(str), argList) match {
    case NumV(num) => num.toString
    case CloV(_, _, _) => "function"
    case RecV(_) => "record"
    case other => other.toString
  }

  def interpret(mrfwae: MRFWAE, env: Map[String, FWAEValue]): FWAEValue = mrfwae match {
    case Num(num) => NumV(num)
    case Add(left, right) => NumV(unwrapNum(interpret(left, env)) + unwrapNum(interpret(right, env)))
    case Sub(left, right) => NumV(unwrapNum(interpret(left, env)) - unwrapNum(interpret(right, env)))
    case With(name, value, body) => interpret(body, env + (name -> interpret(value, env)))
    case Id(name) => if (env.contains(name)) env(name) else error("key not found")
    case App(func, args) => interpret(func, env) match {
      case CloV(params, body, fenv) => {
        if (params.size != args.size) {
          error("wrong arity")
        }
        val range = args.indices.toList
        val temp = range.foldLeft(fenv) {
          (a, i) => a + (params(i) -> interpret(args(i), env))
        }
        interpret(body, temp)
      }
      case NumV(num) => NumV(num)
      case RecV(rec) => RecV(rec)
      case e => error(s"wrong function value: $e")
    }
    case Fun(params, body) => CloV(params, body, env)
    case Rec(rec) => {
      val keys = rec.keys
      val temp = keys.foldLeft(Map[String, FWAEValue]()) {
        (a, i) => a + (i -> interpret(rec(i), env))
      }
      RecV(temp)
    }
    case Acc(expr, name) => interpret(expr, env) match {
      case RecV(rec) => if (rec.contains(name)) rec(name) else error("no such field")
      case _ => error("wrong input for access")
    }
  }

  def unwrapNum(fwae: FWAEValue): Int = fwae match {
    case NumV(num) => num
    case _ => error("not a number")
  }

  def tests: Unit = {
    test(run("{{fun {x y} {+ x y}} 1 2}"), "3")
    test(run("{{fun {} {+ 3 4}}}"), "7")
    testExc(run("{{fun {x y} {+ x y}} 1}"), "wrong arity")
    test(run("{access {record {x 1} {y 2}} x}"), "1")
    testExc(run("{access {record {x 1} {y 2}} z}"), "no such field")
    testExc(run("{record {x {access {record {y 1}} z}}}"), "no such field")
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    test(run("{record {x 1}}"), "record")

    /* Write your own tests */
    test(run("{{fun {x y} {+ x y}} {+ 1 2} {+ 2 3}}"), "8")
    test(run("{with {x 4} {{fun {x y} {+ x y}} {+ 1 2} {+ 2 3}}}"), "8")
    test(run("{with {x 3} {{fun {y z} {+ x y}} {+ 1 2} {+ 2 3}}}"), "6")
    test(run("{with {x 1} {fun {x y} {+ x y}}}"), "function")
    testExc(run("{with {x 1} {{fun {x y} {+ x y}} x}}"), "wrong arity")
    test(run("{{fun {x} {with {x 1} x}} 2}"), "1")
    test(run("{{fun {z} {3}} 2}"), "3")
    test(run("{{fun {z} {z}} 2}"), "2")
    test(run("{{fun {z} {{fun {x} {x}} 1}} 2}"), "1")
    testExc(run("{{fun {z} {{fun {x y} {+ x y}} 1}} 2}"), "wrong arity")
    testExc(run("{{fun {z} {{fun {x y} {+ x y}} 1 2}} 2 3}"), "wrong arity")
    test(run("{{fun {x} {access {record {y 1}} y}} 2}"), "1")
    testExc(run("{{fun {x} {access {record {y 1}} x}} 2}"), "no such field")
    test(run("{{fun {x} {access {with {x 1} {record {x x}}} x}} 2}"), "1")
    test(run("{access {record {x {access {record {y 1}} y}}} x}"), "1")
    testExc(run("{access {+ 1 2} x}"), "wrong input for access")
    testExc(run("{access {with {x 3} {x}} x}"), "wrong input for access")
    test(run("{with {x 3} {access {record {y x}} y}}"), "3")
    test(run("{with {x 3} {access {record {x x}} x}}"), "3")
    testExc(run("{with {x 3} {access {record {x x}} y}}"), "no such field")
    test(run("{access {record {x {with {x 3} {+ x x}}}} x}"), "6")
    test(run("{access {with {x 1} {record {x x}}} x}"), "1")
    test(run("{access {with {x 1} {with {y 1} {{record {x {+ x y}}}}}} x}"), "2")
    test(run("{access {{fun {x} {record {y 1}}} 1} y}"),"1")
    test(run("{access {{fun {x} {record {y x}}} 1} y}"),"1")
    testExc(run("{access {fun {x} {record {y 1}}} y}"),"wrong input for access")
    testExc(run("{access {+ 1 2} x}"), "wrong input for access")
    test(run("{fun {x} {record {x 1}}}"), "function")
    test(run("{fun {x} {- x y}}"), "function")
    test(run("{record {x {access {record {y 1}} y}}}"), "record")
    testExc(run("{record {x {access {record {x 1}} y}}}"), "no such field")
    testExc(run("{access {record {x 3} {y x}} y}"), "key not found")
    test(run("{access {record {x {{fun {y} {y}} 1}}} x}"), "1")
    test(run("{access {record {x {fun {z} {+ z z}}}} x}"), "function")
    testExc(run("{+ 1 {record {x 1}}}"), "not a number")
    test(run("{+ {with {x 1} x} 1}"), "2")
    test(run("{+ {{fun {x} {x}} 1} 1}"), "2")
    testExc(run("{+ {fun {x} {x}} 1}"), "not a number")
    testExc(run("{+ {{fun {x} {x}} 1 2} 1}"), "wrong arity")
    testExc(run("{+ x y}"), "key not found")
    test(run("{{fun {x y z} {with {x {fun {x} {x z}}} {with {f {fun {x} x}} {with {z y} {z f x}}}}} {fun {x y} {x y}} {fun {x y} {y x}} 42}"), "42")
  }
}
