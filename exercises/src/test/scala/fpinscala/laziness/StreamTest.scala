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
    assert(s.take(2).toList == Stream(1,2).toList)
    assert(s.takeUnfold(2).toList == Stream(1,2).toList)

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

  "map" should "map all elements using a function" in {
    val l: Stream[Int] = Stream(1,2,3)
    val f = (a:Int) => a+1
    assert(l.map(f).toList == List(2,3,4))
  }

  "filter" should "leave in the stream the elements that satisfy the condition" in {
    val l: Stream[Int] = Stream(1,2,3,5)
    val f = (a:Int) => (a-1)%2 ==0
    assert(l.filter(f).toList == List(1,3,5))
  }

  "append" should "append a stream to another one" in {
    val l: Stream[Int] = Stream(1,2,3,5)
    val l2: Stream[Int] = Stream(-1,2)
    assert(l.append(l2).toList == List(1,2,3,5,-1,2))
  }

  "flatMap" should "produce a stream from each element and append it" in {
    val f = (a: Int) => Stream(a+1,a+3,a-1)
    val l: Stream[Int] = Stream(1,2)
    assert(l.flatMap(f).toList == List(2,4,0,3,5,1))
  }

  "unfold" should "produce a stream, from a hidden state" in {
    val f = (a: Int) => (if (a<5) Some((a, a+1)) else None)
    assert(Stream.unfold(1)(f).toList == List(1,2,3,4))
  }

  "mapUnfold" should "behave as map" in {
    val l: Stream[Int] = Stream(1,2,3)
    val f = (a:Int) => a+1
    assert(l.mapUnfold(f).toList == l.map(f).toList)
  }

  "zipWith" should "use two streams as pairs of arguments, passed to a function" in {
    val l = Stream(1,2,3)
    val l2 = Stream(4,1,4,-1)
    val f = (a:Int,b:Int) => a+b
    assert(l.zipWith(l2)(f).toList == Stream(5,3,7).toList)
    assert(l2.zipWith(l)(f).toList == Stream(5,3,7).toList)
  }

  "startsWith" should "output if the stream starts with the given stream" in {
    val ref = Stream(1,4,3,2,5)
    val queryPos = Stream(1,4)
    val queryNeg = Stream(0,2)
    val queryPosLong = Stream(1,4,3,2,5, -1)
    assert(ref.startsWith(queryPos))
    assert(ref.startsWith(queryPosLong))
    assert(!ref.startsWith(queryNeg))
  }

  "tails" should "yield all the end substreams" in {
    val ref = Stream(1,4,5)
    val res = Stream(Stream(1,4,5), Stream(4,5), Stream(5))
    val convert = (s:Stream[Stream[Int]]) => s.map( a=> a.toList).toList
    assert(convert(ref.tails) == convert(res))
  }

  "zipAll" should "use options until exhausted" in {
    val l = Stream(1,2)
    val l2 = Stream(4,5,6)
    assert(l.zipAll(l2).toList == Stream((Some(1), Some(4)), (Some(2), Some(5)), (None, Some(6))).toList)
  }

  "scanRight" should "show the successive operations performed on a stream" in {
    val l = Stream(1,2,3)
    val res = Stream(6, 5, 3, 0)
    assert(l.scanRight(0)((a,b) =>a+b).toList == res.toList)
  }
}
