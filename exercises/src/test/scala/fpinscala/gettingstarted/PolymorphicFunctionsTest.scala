package fpinscala.gettingstarted

import org.scalatest.FlatSpec

class PolymorphicFunctionsTest extends FlatSpec {

  "isSorted" should "verify if an array is sorted" in {
    assert(PolymorphicFunctions.isSorted(Array(2, 3, 6, 7, 9), (a: Int,b: Int) => a > b))
  }

}
