package cs320

package object hw09 extends Homework09 {
  type Env = Map[String, CORELValue]

  case class TypeEnv(vars: Map[String, Type] = Map(),
                     tbinds: Map[String, Map[String, Type]] = Map()) {
    def addVar(x: String, t: Type): TypeEnv = copy(vars = vars + (x -> t))

    def addTBind(x: String, cs: Map[String, Type]): TypeEnv = copy(tbinds = tbinds + (x -> cs))
  }

  trait CORELValue

  case class NumV(n: Int) extends CORELValue

  case class BoolV(b: Boolean) extends CORELValue

  case class CloV(param: String, body: COREL, var env: Env) extends CORELValue

  case class VariantV(name: String, value: CORELValue) extends CORELValue

  case class ConstructorV(name: String) extends CORELValue

  val argMap: Env = Map.empty[String, CORELValue]
  val typeMap: TypeEnv = TypeEnv(Map.empty[String, Type], Map.empty[String, Map[String, Type]])

  def typeCheck(str: String): Type = checkHelper(COREL.apply(str), typeMap)

  def interp(str: String): String = interpHelper(COREL.apply(str), argMap) match {
    case NumV(num) => num.toString
    case BoolV(b) => b.toString
    case CloV(_, _, _) => "function"
  }

  def interpHelper(corel: COREL, env: Env): CORELValue =
    corel match {
      case Num(num) => NumV(num)
      case Bool(b) => BoolV(b)
      case Add(l, r) => NumV(unwrapNum(interpHelper(l, env)) + unwrapNum(interpHelper(r, env)))
      case Sub(l, r) => NumV(unwrapNum(interpHelper(l, env)) - unwrapNum(interpHelper(r, env)))
      case Equ(l, r) => BoolV(interpHelper(l, env) == interpHelper(r, env))
      case With(name, ty, expr, body) => interpHelper(body, env + (name -> interpHelper(expr, env)))
      case Id(name) => env.getOrElse(name, error("key not found"))
      case Fun(param, paramty, body) => CloV(param, body, env)
      case App(funE, argE) => interpHelper(funE, env) match {
        case CloV(param, body, fenv) =>
          interpHelper(body, fenv + (param -> interpHelper(argE, env)))
        case ConstructorV(name) => VariantV(name, interpHelper(argE, env))
        case n@NumV(_) => n
        case b@BoolV(_) => b
        case e => error(s"wrong function value: $e")
      }
      case IfThenElse(testE, thenE, elseE) => interpHelper(testE, env) match {
        case BoolV(true) => interpHelper(thenE, env)
        case BoolV(false) => interpHelper(elseE, env)
        case _ => error("not a boolean")
      }
      case Rec(fname, fty, pname, pty, body) =>
        val cloV = CloV(pname, body, env)
        cloV.env = env + (fname -> cloV)
        cloV
      case WithType(_, constructors, body) =>
        val extendedEnv = constructors.keys.foldLeft(env) {
          (a, c) => a + (c -> ConstructorV(c))
        }
        interpHelper(body, extendedEnv)
      case Cases(_, dispatchE, cases) => interpHelper(dispatchE, env) match {
        case VariantV(vname, av) =>
          if (cases.contains(vname)) {
            interpHelper(cases(vname)._2, env + (cases(vname)._1 -> av))
          } else {
            error(s"$vname is a free constructor")
          }
        case v => error(s"not a variant: $v")
      }
    }

  def checkHelper(corel: COREL, tEnv: TypeEnv): Type = corel match {
    case Num(_) => NumT
    case Bool(_) => BoolT
    case Add(left, right) =>
      mustSame(checkHelper(left, tEnv), NumT)
      mustSame(checkHelper(right, tEnv), NumT)
      NumT
    case Sub(left, right) =>
      mustSame(checkHelper(left, tEnv), NumT)
      mustSame(checkHelper(right, tEnv), NumT)
      NumT
    case Equ(left, right) =>
      mustSame(checkHelper(left, tEnv), NumT)
      mustSame(checkHelper(right, tEnv), NumT)
      BoolT
    case With(name, ty, expr, body) =>
      mustSame(checkHelper(expr, tEnv), ty)
      checkHelper(body, tEnv.addVar(name, ty))
    case Id(name) =>
      tEnv.vars.getOrElse(name, error(s"no type: $name is a free identifier"))
    case Fun(param, paramty, body) =>
      validType(paramty, tEnv)
      ArrowT(paramty, checkHelper(body, tEnv.addVar(param, paramty)))
    case App(funE, argE) =>
      val funT = checkHelper(funE, tEnv)
      val argT = checkHelper(argE, tEnv)
      println(funT, argT)
      funT match {
        case ArrowT(param, result) if same(argT, param) => result
        case _ => error(s"no type: apply $argT to $funT")
      }
    case IfThenElse(testE, thenE, elseE) =>
      println(s"if: $testE, $thenE, $elseE")
      mustSame(checkHelper(testE, tEnv), BoolT)
      mustSame(checkHelper(thenE, tEnv), checkHelper(elseE, tEnv))
    case Rec(fname, fty, pname, pty, body) =>
      mustSame(fty, ArrowT(pty, checkHelper(body, tEnv.addVar(fname, fty).addVar(pname, pty))))
    case WithType(name, constructors, body) =>
      val extendedEnv = constructors.foldLeft(tEnv.addTBind(name, constructors)) {
        (a, constructor) => a.addVar(constructor._1, ArrowT(constructor._2, IdT(name)))
      }
      constructors.foreach(p => {
        validType(p._2, extendedEnv)
      })
      checkHelper(body, extendedEnv)
    case Cases(name, dispatchE, cases) =>
      val constructors = tEnv.tbinds.getOrElse(name, error(s"no type: $name is a free type"))
      if (cases.keys.size != constructors.size) error(s"not all cases for: $name")
      mustSame(checkHelper(dispatchE, tEnv), IdT(name))
      val keys = cases.keys
      val firstType = checkHelper(cases(keys.head)._2, tEnv.addVar(cases(keys.head)._1, constructors.getOrElse(keys.head, error(s"no type: ${keys.head} is free"))))
      keys.foldLeft(firstType) {
        (a, c) =>
          val caseType = checkHelper(cases(c)._2, tEnv.addVar(cases(c)._1, constructors.getOrElse(c, error(s"no type: $c is free"))))
          mustSame(a, caseType)
      }
  }

  def mustSame(left: Type, right: Type): Type =
    if (same(left, right)) left
    else error(s"no type: $left is not equal to $right")

  def same(left: Type, right: Type): Boolean = (left, right) match {
    case (NumT, NumT) => true
    case (BoolT, BoolT) => true
    case (IdT(a), IdT(b)) => a == b
    case (ArrowT(p1, r1), ArrowT(p2, r2)) => same(p1, p2) && same(r1, r2)
    case _ => false
  }

  def validType(ty: Type, tyEnv: TypeEnv): Type = ty match {
    case NumT => ty
    case BoolT => ty
    case ArrowT(p, r) => ArrowT(validType(p, tyEnv), validType(r, tyEnv))
    case IdT(x) =>
      if (tyEnv.tbinds.contains(x)) ty
      else error(s"no type: $x is a free type")
  }

  def unwrapNum(corel: CORELValue): Int = corel match {
    case NumV(num) => num
    case _ => error("not a number")
  }

  def tests: Unit = {
    test(run("42"), "42")
    test(run("true"), "true")
    test(run("{+ 1 2}"), "3")
    test(run("{- 2 1}"), "1")
    test(run("{= 1 0}"), "false")
    testExc(run("{= true 0}"), "no type")
    test(run("{with {x : num 1} x}"), "1")
    test(run("{{fun {x : num} {+ x 1}} 2}"), "3")
    test(run(
      """
              {{recfun {f: {num -> num} x: num}
                       {if {= x 0} 0 {+ {f {- x 1}} x}}}
               10}"""), "55")
    testExc(run("{if 1 2 3}"), "no type")
    test(run(
      """
              {withtype
                {fruit {apple num}
                       {banana num}}
                {cases fruit {apple 1}
                       {apple {x} x}
                       {banana {y} y}}}"""), "1")
    testExc(run(
      """
          {withtype
            {fruit {apple num}
                   {banana num}}
            {cases fruit {apple 1}
                   {apple {x} x}}}"""), "not all cases")

    /* Write your own tests */
    testExc(run("{{fun {x: num} x} true}"), "no type")
    testExc(run("{with {x: num true} x}"), "no type")
    testExc(run("{with {x: bool true} {+ 1 x}}"), "no type")
    test(run("{{fun {x: num} {+ x 1}} {with {y:num 45} y}}"), "46")
    test(run(
      """
         {with {x : num 4}
          {if {= x 4}
            {{fun {y: num} y} x}
            {{fun {y: num} {+ y 1}} x}
          }
        }
      """), "4")
    test(run("{with {x: num {{fun {y:num} {+ y y}} 2}} {+ x x}}"), "8")
    testExc(run("{with {x: bool {{fun {y:num} {+ y y}} 2}} {+ x x}}"), "no type")
    test(run(
      """
            {withtype
                {fruit {apple num}
                        {banana num}}
                {cases fruit {apple 1}
                        {apple {x} {if {= x 1} 23 45}}
                        {banana {x} x}
                }
            }
      """), "23")
    testExc(run(
      """
            {withtype
                {fruit {apple bool}
                        {banana bool}}
                {cases fruit {apple true}
                        {apple {x} {if {= x 1} 23 45}}
                        {banana {x} x}
                }
            }
      """), "no type")
    test(run(
      """
            {withtype
                {fruit {apple bool}
                        {banana bool}}
                {cases fruit {apple true}
                        {apple {x} {if x {with {x:num 1} x} 45}}
                        {banana {x} 6}
                }
            }
      """), "1")
  }
}
