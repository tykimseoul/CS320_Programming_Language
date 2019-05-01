package cs320

package object hw07 extends Homework07 {
  type Env = Map[String, KXCFAEValue]

  trait KXCFAEValue

  case class NumV(n: Int) extends KXCFAEValue

  case class CloV(param: String, body: KXCFAE, env: Env) extends KXCFAEValue

  case class ContV(proc: Cont) extends KXCFAEValue

  type Cont = KXCFAEValue => KXCFAEValue

  def run(str: String): String = ???

  def interpret(kxcfae: KXCFAE, env: Env, k: Cont): KXCFAEValue = kxcfae match {
    case App(f, a) =>
      interpret(f, env, fv =>
        interpret(a, env, av =>
          fv match {
            case CloV(p, b, fenv) => interpret(b, fenv + (p -> av), k)
            case ContV(kv) => kv(av)
            case v => error(s"not a closure: $v")
          }))
    case Withcc(x, b) => interpret(b, env + (x -> ContV(k)), k)
  }

  def tests: Unit = {
    test(run("{{fun {x y} {- y x}} 10 12}"), "2")
    test(run("{fun {} 12}"), "function")
    testExc(run("{{fun {x y} 1} 2}"), "wrong arity")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")
    test(run("{{fun {x y} {- y x}} 10 12}"), "2")
    test(run("{fun {} 12}"), "function")
    testExc(run("{{fun {x y} 1} 2}"), "wrong arity")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")

    /* Write your own tests */
  }
}
