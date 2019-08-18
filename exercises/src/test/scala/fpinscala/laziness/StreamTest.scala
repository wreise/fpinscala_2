package fpinscala.laziness

import org.scalatest.FlatSpec

class StreamTest extends FlatSpec {

  behavior of "StreamTest"

  "drop" should "drop the first n values" in {
    val sInter: Stream[Int] = Stream(5,3)
    /*val sOuter: Stream[Int] = Cons(()=> 1, Cons(()=>2 , Cons(3: =>Int, sInter)))*/
    val s: Stream[Int] = Stream(1,2,3,5,3)
    /*assert(s.drop(3) == Stream(5,3))*/
    /*How to test for the equality of two streams ?! They are compared via their memory, and as objects, how can you make them the same?*/
    assert(s.drop(3).toList == Stream(5,3).toList)
    assert(s.drop(0) == s)
    assert(s.drop(6) == Empty)

  }

  "forAll" should "verify if all elements satisfy a condition" in {
    val l: Stream[Int] = Stream(1,2,4,-1)
    val f1 = (a:Int)=> (a<2)
    val f2 = (a:Int)=> (a<5)

    assert(!l.forAll(f1))
    assert(l.forAll(f2))

  }

  "takeWhile" should "take elements while the condition is true" in {
    val f = (a:Int) => (a<=2)
    val l: Stream[Int]  = Stream(1,2,-1, 4, 1, 0)

    /*assert( l.takeWhile(f) == Stream(1,2,-1))*/
    assert( l.takeWhile(f).toList == Stream(1,2,-1).toList)
    assert( l.takeWhile_fold(f).toList == Stream(1,2,-1).toList)

  }

  "toList" should "convert a Stream to a List" in {
    val s: Stream[Int] = Stream(1,2,3)
    assert(s.toList == List(1,2,3))
    /*assert(s.toList_fold == List(1,2,3))*/
  }

  "take" should "take the first elements" in {
    val s: Stream[Int] = Stream(1,2,3)
    assert(s.take(2) == Stream(1,2))

  }

  "headOption" should "return an Option on the first element, or None if empty stream" in {
    val l:Stream[Int] = Stream(1,2,3)
    val l2: Stream[Int] = Stream()

    assert(l.headOption == Some(1))
    assert(l2.headOption == (None: Option[Int]))
  }

  "constant" should "produce a constant, infinite stream of elements" in {
    assert(Stream.constant[Int](2).take(3).toList == List(2,2,2))
  }

  "from" should "produce an increasing sequence" in {
    assert(Stream.from(2).take(3).toList == List(2,3,4))
  }

  it should "startsWith" in {

  }

}
