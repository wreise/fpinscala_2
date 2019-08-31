package fpinscala.errorhandling

import org.scalatest.FlatSpec

class EitherTest extends FlatSpec {

  /*behavior of "EitherTest"

  it should "map" in {

  }

  it should "map2" in {

  }

  it should "orElse" in {

  }

  it should "flatMap" in {

  }*/

  "map" should "apply the function if possible, otherwise, carry the error" in {
    val i1: Either[String, Double] = Right(2.0)
    val e1: Either[String, Double] = Left("Whatever")
    val f: (Double => Double) = (a:Double) => a+1

    assert(i1.map(f) == Right(3.0))
    assert(e1.map(f) == Left("Whatever"))
  }

}
