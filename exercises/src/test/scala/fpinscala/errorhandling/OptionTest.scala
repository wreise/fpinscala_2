package fpinscala.errorhandling

import org.scalatest.FlatSpec

class OptionTest extends FlatSpec {

  /*behavior of "OptionTest"

  it should "map2" in {

  }

  it should "sequence" in {

  }

  it should "traverse" in {

  }

  it should "variance" in {

  }

  it should "mean" in {

  }*/

  "map2" should "map two options into one" in {
    val a1: Option[Int] = Some(2)
    val a2: Option[Int] = Some(3)
    val aNone: Option[Int] = None

    assert(Option.map2(a1, a2)((a,b) => a + b) == Some(5))
    assert(Option.map2(a1, aNone)((a,b) => a + b) == None)
    assert(Option.map2(aNone, a1)((a,b) => a + b) == None)
  }

  "sequence" should "convert a list of options into an option on a list" in {
    val listGood: fpinscala.datastructures.List[Option[Int]] = fpinscala.datastructures.List(Some(1), Some(2), Some(-1))
    val listBad: fpinscala.datastructures.List[Option[Int]] = fpinscala.datastructures.List(Some(1), None, Some(-1))

    assert(Option.sequence(listGood) == Some(fpinscala.datastructures.List(1,2,-1)))
    assert(Option.sequence(listBad) == None)
  }

  "traverse" should "apply a function to all of its elements" in {
    val l: fpinscala.datastructures.List[Double] = fpinscala.datastructures.List(1, 3, 0, 2)
    assert(Option.traverse(l)((a: Double) => Some(a+1.0): Option[Double]) == Some(fpinscala.datastructures.List(2,4,1,3)))
    assert(Option.traverse(l)((a: Double) => (if (a!= 0) Some(1/a) else None)) == None)
  }

}
