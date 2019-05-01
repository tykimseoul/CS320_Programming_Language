package cs320

package object hw05 extends Homework05 {
  type Addr = Int
  type Sto = Map[Addr, BFAEValue]
  type Env = Map[String, Addr]

  trait BFAEValue

  case class NumV(n: Int) extends BFAEValue

  case class CloV(param: String, body: SRBFAE, env: Env) extends BFAEValue

  case class BoxV(address: Addr) extends BFAEValue

  case class RecV(records: Map[String, Addr]) extends BFAEValue

  val argMap: Env = Map.empty[String, Addr]
  val storageMap: Sto = Map.empty[Addr, BFAEValue]

  def run(str: String): String = interpret(SRBFAE.apply(str), argMap, storageMap)._1 match {
    case NumV(num) => num.toString
    case CloV(_, _, _) => "function"
    case BoxV(_) => "box"
    case RecV(_) => "record"
  }

  def interpret(srbfae: SRBFAE, env: Env, storage: Sto): (BFAEValue, Sto) = srbfae match {
    case Num(num) => (NumV(num), storage)
    case Add(left, right) =>
      val (lv, ls) = interpret(left, env, storage)
      val (rv, rs) = interpret(right, env, ls)
      (NumV(unwrapNum(lv) + unwrapNum(rv)), rs)
    case Sub(left, right) =>
      val (lv, ls) = interpret(left, env, storage)
      val (rv, rs) = interpret(right, env, ls)
      (NumV(unwrapNum(lv) - unwrapNum(rv)), rs)
    case Id(name) => (storageLookUp(environmentLookUp(name, env), storage), storage)
    case App(f, a) => a match {
      case Id(name) =>
        val (fv, fs) = interpret(f, env, storage)
        fv match {
          case CloV(x, b, fenv) =>
            val addr = environmentLookUp(name, env)
            interpret(b, fenv + (x -> addr), fs)
          case _ => error(s"not a closure: $fv")
        }
      case _ =>
        val (fv, fs) = interpret(f, env, storage)
        val (av, as) = interpret(a, env, fs)
        fv match {
          case CloV(x, b, fenv) =>
            val addr = malloc(as)
            interpret(b, fenv + (x -> addr), as + (addr -> av))
          case _ => error(s"not a closure: $fv")
        }
    }
    case Fun(param, body) => (CloV(param, body, env), storage)
    case NewBox(expr) =>
      val (v, s) = interpret(expr, env, storage)
      val addr = malloc(s)
      (BoxV(addr), s + (addr -> v))
    case SetBox(boxE, valE) =>
      val (bv, bs) = interpret(boxE, env, storage)
      bv match {
        case BoxV(addr) =>
          val (v, s) = interpret(valE, env, bs)
          (v, s + (addr -> v))
        case _ => error(s"not a box: $bv")
      }
    case OpenBox(boxE) =>
      val (bv, bs) = interpret(boxE, env, storage)
      bv match {
        case BoxV(addr) => (storageLookUp(addr, bs), bs)
        case _ => error(s"not a box: $bv")
      }
    case Seqn(left, right) =>
      val (_, ls) = interpret(left, env, storage)
      if (right.size > 1) {
        interpret(Seqn(right.head, right.drop(1)), env, ls)
      } else {
        interpret(right.head, env, ls)
      }
    case Rec(fields) =>
      val keys = fields.keys
      val temp = keys.foldLeft((env, storage)) {
        (a, key) =>
          val (rv, rs) = interpret(fields(key), env, a._2)
          val address = malloc(rs)
          val (c: Env, b) = (a._1 + (key -> address), rs + (address -> rv))
          (c, b)
      }
      (RecV(temp._1), temp._2)
    case Get(record, field) =>
      val (gv, gs) = interpret(record, env, storage)
      gv match {
        case RecV(records) =>
          if (records.contains(field)) (gs(records(field)), gs)
          else error("no such field in record")
        case _ => error(s"not a record: $gv")
      }
    case Set(record, field, expr) =>
      val (sv, ss) = interpret(record, env, storage)
      sv match {
        case RecV(records) =>
          if (records.contains(field)) {
            val (ev, es) = interpret(expr, env, ss)
            (ev, es + (records(field) -> ev))
          } else {
            error("no such field in record")
          }
        case _ => error(s"not a record: $sv")
      }
  }

  def unwrapNum(bfae: BFAEValue): Int = bfae match {
    case NumV(num) => num
    case _ => error("not a number")
  }

  def malloc(sto: Sto): Addr = sto.keySet.+(0).max + 1

  def storageLookUp(addr: Addr, storage: Sto): BFAEValue = {
    if (storage.contains(addr)) storage(addr) else error(s"storage key not found: $addr")
  }

  def environmentLookUp(x: String, env: Env): Addr = {
    if (env.contains(x)) env(x) else error(s"env key not found: $x")
  }

  def tests: Unit = {
    test(run(
      """{{fun {b} {seqn {setbox b {+ 2 {openbox b}}}
                              {setbox b {+ 3 {openbox b}}}
                              {setbox b {+ 4 {openbox b}}}
                              {openbox b}}}
                    {newbox 1}}"""), "10")
    testExc(run("{get {rec {x 1}} y}"), "no such field")
    test(run("{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}}"), "5")
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    test(run("{newbox 1}"), "box")
    test(run("{rec}"), "record")
    //
    //    /* Write your own tests */
    test(run("{{fun {b} {openbox b}} {newbox 10}}"), "10")
    test(run("{{fun {b} {openbox b}} {newbox {+ 1 2}}}"), "3")
    test(run("{{fun {b} {openbox b}} {newbox {{fun {c} {openbox c}} {newbox 4}}}}"), "4")
    testExc(run("{get {set {rec {x 1}} x 2 } x}"), "not a record")
    test(run("{rec {x 3} {y 2} {z 1}}"), "record")
    test(run("{openbox {{fun {a} {newbox 2}} {newbox 1}}}"), "2")
    test(run("{{fun {b} {seqn {setbox b {+ 2 {openbox b}}} {openbox b}}} {newbox 1}}"), "3")
    test(run("{get{rec {x {newbox 1}}} x}"), "box")
    test(run("{get{rec {x {openbox {newbox 1}}}} x}"), "1")
    test(run("{get{rec {x {openbox {newbox {openbox {newbox 23}}}}}} x}"), "23")
    test(run("{openbox {{fun {b} {get {rec {x b}} x}} {newbox 1}}}"), "1")
    test(run(" {{  fun {b} { get { rec { x {setbox b 3} }  }  x } }  {newbox 1}} "), "3")
    test(run(" { openbox {{  fun {b} { get { rec { x {setbox b 3}} {y b}  }  y } }  {newbox 1}} } "), "3")
    testExc(run(" { openbox {{  fun {b} { get { rec { x {setbox b 3}} {y b}  }  y } }  45} } "), "not a box")
    test(run("{{fun {r} {seqn {{fun {s} {get s x}} {rec {x {set r x 3}}}} {get r x} }} {rec {x 1} {y 2}}}"), "3")
    test(run("{openbox        {get      {rec {x {get {rec {y {newbox 34}}} y}}}      x}        }"), "34")
    test(run(" {{  fun {b} { get { rec { x {openbox {get {rec {y {newbox 34}}} y}} }  }  x } }  {newbox 1}} "), "34")
    test(run("{get {rec {x 2} {x 3}} x}"), "3")
    test(run("{get{rec {x {{  fun {b} { get { rec { x {setbox b 3} }  }  x } }  {newbox 1}}}} x}"), "3")
    test(run("{{fun {x} {get {rec {x 1} {y 2}} x}} {newbox 32}}"), "1")
    testExc(run("{get {rec {x 2} {y 4}} z}"), "no such field in record")
    test(run("{{fun {b} {setbox b {+ {openbox b} 3}}} {newbox 3}}"), "6")
    test(run("{{fun {b} {get { rec {b {setbox b {+ {openbox b} 3}}}} b} } {newbox 3}}"), "6")
    test(run("{openbox {newbox {get {rec {x {newbox 23}}} x}}}"), "box")
    test(run("{openbox {openbox {newbox {get {rec {x {newbox 23}}} x}}}}"), "23")
    test(run("{openbox {get {openbox {newbox {rec {x {newbox 23}}}}} x}}"), "23")
    test(run("{{get {rec {x {fun {b} b}}} x} 3}"), "3")
  }
}