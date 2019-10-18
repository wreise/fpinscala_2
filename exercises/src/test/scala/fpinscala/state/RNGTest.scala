package fpinscala.state

import org.scalatest.FlatSpec

class RNGTest extends FlatSpec {

  behavior of "RNGTest"

  object RNGTest{
    case class Constant(s: Int) extends RNG {
      def nextInt: (Int, RNG) = (s, Constant(s): RNG)
    }

    case class ListOfConstants(l: List[Int]) extends RNG {
      def nextInt: (Int, RNG) = (l.head, ListOfConstants(l.tail))
    }
  }

  "nonNegativeInt" should "generate a non negative int" in {

    val rng1 = RNGTest.Constant(1)
    val rng2 = RNGTest.Constant(-2)
    val rng3 = RNGTest.Constant(-Int.MinValue)

    assert(RNG.nonNegativeInt(rng1) == (1, RNGTest.Constant(1)))
    assert(RNG.nonNegativeInt(rng2) == (2, rng2))
    assert(RNG.nonNegativeInt(rng3) == (0,rng3))
    // HOW TO TEST ALL THE CASES ?! The 'Simple' generator does allow us to
  }

  "ints" should "generate a list of Ints" in {
    val rng1 = RNGTest.ListOfConstants(List(2,3,5))
    val rng2 = RNGTest.ListOfConstants(List(5))
    assert(RNG.ints(2)(rng1) == (List(2,3), rng2))
  }

  "map" should "change the type of a generator" in {
    val rngA = RNG.Simple(2)
    assert( RNG.map(RNG.unit(1))((a:Int) => a.toDouble/2)(rngA) == RNG.unit(0.5)(rngA))

    assert( RNG.mapWithFlatMap(RNG.unit(1))((a:Int) => a.toDouble/2)(rngA) == RNG.unit(0.5)(rngA))
  }


  "map2" should "generates elements by combining 2 generators" in {
    val rngA = RNG.Simple(20) // we need something to evaluate the functions at. Otherwise, it fails.
    // The advantage is that since we use RNG.unit, the result doesn't depend on what we put inside.
    assert(RNG.map2( RNG.unit(1), RNG.unit(2))( (a:Int,b:Int)=> a+b)(rngA) == RNG.unit(3)(rngA))
    assert(RNG.map2WithFlatMap( RNG.unit(1), RNG.unit(2))( (a:Int,b:Int)=> a+b)(rngA) == RNG.unit(3)(rngA))
  }

  it should "flatMap" in {
  }

  "nonNegativeLessThan" should "generate a RN less than n" in {
    val rng1 = RNGTest.Constant(2)
    assert(RNG.nonNegativeLessThan(10)(rng1) == (2, rng1))

    val rngList = RNGTest.ListOfConstants(List(Int.MaxValue-2, Int.MaxValue-7, 12))
    assert(RNG.nonNegativeLessThan(10)(rngList) == (2, RNGTest.ListOfConstants(List())))
  }

  "sequence" should "combine a list of generators into a generator of lists" in {
    val rngSimple = RNG.Simple(-100)
    val rng1 = RNG.unit(2)
    val rng2 = RNG.unit(3)
    val rng3 = RNG.unit(17)

    assert(RNG.sequence(List(rng1, rng2, rng3))(rngSimple) == RNG.unit(List(2, 3, 17))(rngSimple))
    assert(RNG.sequenceFoldRight(List(rng1, rng2, rng3))(rngSimple) == RNG.unit(List(2, 3, 17))(rngSimple))
  }

  "simulateMachine" should "simulate the candy machine and count the outputs" in {
    val initialState = Machine(false, 5, 10)
    val l = State.simulateMachine(List(Turn, Turn, Coin, Turn, Coin, Turn)).run(initialState)
    //print(l)
    assert(l._1 == (2,12))
  }

}
