package cs320

package object hw09 extends Homework09 {
  def typeCheck(str: String): Type = ???
  def interp(str: String): String = ???

  def tests: Unit = {
    test(run("42"), "42")
    test(run("true"), "true")
    test(run("{+ 1 2}"), "3")
    test(run("{- 2 1}"), "1")
    test(run("{= 1 0}"), "false")
    testExc(run("{= true 0}"), "no type")
    test(run("{with {x : num 1} x}"), "1")
    test(run("{{fun {x : num} {+ x 1}} 2}"), "3")
    test(run("""
      {{recfun {f: {num -> num} x: num}
               {if {= x 0} 0 {+ {f {- x 1}} x}}}
       10}"""), "55")
    testExc(run("{if 1 2 3}"), "no type")
    test(run("""
      {withtype
        {fruit {apple num}
               {banana num}}
        {cases fruit {apple 1}
               {apple {x} x}
               {banana {y} y}}}"""), "1")
    testExc(run("""
      {withtype
        {fruit {apple num}
               {banana num}}
        {cases fruit {apple 1}
               {apple {x} x}}}"""), "not all cases")

    /* Write your own tests */
  }
}
