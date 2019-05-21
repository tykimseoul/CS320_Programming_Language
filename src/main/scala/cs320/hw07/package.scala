package cs320

package object hw07 extends Homework07 {
  type Env = Map[String, KXCFAEValue]

  trait KXCFAEValue

  case class NumV(n: Int) extends KXCFAEValue

  case class CloV(param: List[String], body: KXCFAE, env: Env) extends KXCFAEValue

  case class ContV(proc: Cont) extends KXCFAEValue

  case class ListContV(proc: ListCont) extends KXCFAEValue

  case class ThrowV() extends KXCFAEValue

  type Cont = KXCFAEValue => KXCFAEValue
  type ListCont = List[KXCFAEValue] => KXCFAEValue

  def run(str: String): String = interpret(KXCFAE.apply(str), Map.empty[String, KXCFAEValue], k => k) match {
    case NumV(num) => num.toString
    case CloV(_, _, _) => "function"
    case ThrowV() => error("no enclosing try-catch")
  }

  def interpret(kxcfae: KXCFAE, env: Env, k: Cont): KXCFAEValue = {
    //    println(s"in interp: $kxcfae")
    kxcfae match {
      case Num(n) => k(NumV(n))
      case Add(l, r) =>
        interpret(l, env, {
          case lv@ThrowV() => k(lv)
          case lv =>
            interpret(r, env, {
              case rv@ThrowV() => k(rv)
              case rv => k(NumV(unwrapNum(lv) + unwrapNum(rv)))
            })
        })
      case Sub(l, r) =>
        interpret(l, env, {
          case lv@ThrowV() => k(lv)
          case lv =>
            interpret(r, env, {
              case rv@ThrowV() => k(rv)
              case rv => k(NumV(unwrapNum(lv) - unwrapNum(rv)))
            })
        })
      case Id(x) => k(env.getOrElse(x, error(s"free identifier: $x")))
      case Fun(p, b) => k(CloV(p, b, env))
      case App(f, args) =>
        interpret(f, env, fv => {
          def interpretList(kxcfaeList: List[KXCFAE], kxcfaeValueList: List[KXCFAEValue]): KXCFAEValue = {
            kxcfaeList match {
              case head :: rest => interpret(head, env, {
                case h@ThrowV() => k(h)
                case h => interpretList(rest, kxcfaeValueList :+ h)
              })
              case Nil =>
                fv match {
                  case CloV(params, b, fenv) =>
                    if (params.size != args.size) {
                      error("wrong arity")
                    }
                    val range = args.indices.toList
                    val temp = range.foldLeft(fenv) {
                      (a, i) => a + (params(i) -> kxcfaeValueList(i))
                    }
                    interpret(b, temp, k)
                  case ContV(kv) =>
                    if (args.length != 1) {
                      error("wrong arity for continuation")
                    }
                    kv(kxcfaeValueList.head)
                  case ThrowV() => k(fv)
                  case v => error(s"not a closure: $v")
                }
            }
          }

          interpretList(args, List.empty[KXCFAEValue])
        })
      //          interpretList(args, env, avs => {
      //            println(s"in: $fv, $avs")
      //            fv match {
      //              case CloV(params, b, fenv) =>
      //                if (params.size != args.size) {
      //                  error("wrong arity")
      //                }
      //                val range = args.indices.toList
      //                val temp = range.foldLeft(fenv) {
      //                  (a, i) => a + (params(i) -> avs(i))
      //                }
      //                val result = interpret(b, temp, k)
      //                println(s"$avs, $result")
      //                result
      //              case ListContV(kv) =>
      //                println("listcontv")
      //                kv(avs)
      //              case ContV(kv) =>
      //                println(s"contv avs: $avs")
      //                kv(avs.head)
      //              case v => error(s"not a closure: $v")
      //            }
      //          })
      case If0(cond, thenE, elseE) =>
        interpret(cond, env, {
          case NumV(0) => interpret(thenE, env, k)
          case i@ThrowV() => k(i)
          case _ => interpret(elseE, env, k)
        })
      case Withcc(x, b) => interpret(b, env + (x -> ContV(k)), k)
      case Try(tryE, catchE) =>
        interpret(tryE, env, {
          case ThrowV() => interpret(catchE, env, k)
          case t => k(t)
        })
      case Throw => k(ThrowV())
    }
  }

  //  def interpretList(kxcfaeList: List[KXCFAE], env: Env, k: ListCont): KXCFAEValue = {
  //    println(s"interpret list $kxcfaeList")
  //    val temp = kxcfaeList.foldLeft(List.empty[KXCFAEValue]) {
  //      (a, i) => a :+ interpret(i, env, k => k);
  //    }
  //    println(s"temp: $kxcfaeList -> $temp")
  //    k(temp)
  //  }


  def unwrapNum(kxcfaeValue: KXCFAEValue): Int = kxcfaeValue match {
    case NumV(num) => num
    case _ => error("not a number")
  }

  def tests: Unit = {
    test(run("{{fun {x y} {- y x}} 10 12}"), "2")
    test(run("{fun {} 12}"), "function")
    testExc(run("{{fun {x y} 1} 2}"), "wrong arity")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")
    test(run("{try 1 catch 2}"), "1")
    test(run("{try {throw} catch 2}"), "2")
    test(run("{try {+ 1 {throw}} catch 2}"), "2")
    test(run("{{fun {f} {try {f} catch 1}} {fun {} {throw}}}"), "1")
    testExc(run("{throw}"), "no enclosing try-catch")

    /* Write your own tests */
    test(run("{ {fun {} 1} }"), "1")
    test(run("{try {+ {throw} 1} catch 2}"), "2")
    testExc(run("{{fun {x y} x} 1 {throw}}"), "no enclosing try-catch")
    test(run("{try {try 1 catch 2} catch 3}"), "1")
    testExc(run("{withcc esc {{fun {x y} x} 1 {esc 3 4}}}"), "wrong arity for continuation")
    test(run("{if0 0 3 6}"), "3")
    test(run("{try {if0 {- 1 1} {try {+ {throw} 1} catch 2} 4} catch 6}"), "2")
    test(run("{withcc esc {{fun {x y} {try {+ x {throw}} catch {throw}}} 1 {esc 3}}}"), "3")
    testExc(run("{withcc esc {{fun {x y} {esc {try {+ x {throw}} catch {throw}}}} 1 3}}"), "no enclosing try-catch")
    test(run("{+ {withcc k {k 5}} 3}"), "8")
    test(run("{{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} 1 {+ y {g g {- y 1}}}}} 5}"), "15")
    test(run("{withcc done {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {done 6} {+ y {g g {- y 1}}}}} 10}}"), "6")
    test(run("{withcc k {- 0 {k 10}}}"), "10")
    test(run("{{fun {a b c} {- {+ {withcc k {+ {k 100} a}} b} c}} 100 200 100}"), "200")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 8}}}"), "8")
    test(run("{{withcc esc {{fun {x y} {fun {z} {+ z y}}} 1 {withcc k {esc k}}}} 10}"), "20")
    test(run("{try {{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {+ y {g g {- y 1}}}}} 10} catch 8}"), "8")
    test(run("{{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch y}}} 6}"), "20")
    test(run("{+ {try {- 10 {throw}} catch 6} 10}"), "16")
    test(run("{try {if0 {throw} 3 4} catch 5}"), "5")
    test(run("{try {{fun {x y} {try x catch y}} {throw} 0} catch -1}"), "-1")
    test(run("{try {withcc xxx {{fun {x y z w} {+ {+ x y} {+ z w}}} 1 2 {throw} {xxx 10}}} catch 42}"), "42")
    test(run("{withcc esc {try {+ {throw} {esc 3}} catch 4}}"), "4")
    test(run("{+ 12 {withcc k {+ 1 {k {{fun {} 6}}}}}}"), "18")
  }
}
