package fpinscala.gettingstarted

import org.scalatest.FlatSpec

class MyModuleTest extends FlatSpec {

  "A fib function" should "compute correctly fib(5)" in {
    assert(MyModule.fib(1) == 1)
  }

  "A fib function" should "compute correctly fib(20)" in {
    assert(MyModule.fib(20) == 6765)
  }

}
