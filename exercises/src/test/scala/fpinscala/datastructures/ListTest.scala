package fpinscala.datastructures

import org.scalatest.FlatSpec

class ListTest extends FlatSpec {

  "tail" should "return a new list without the fist element" in {
    assert(List.tail(Cons(1, Cons(2, Cons(3, Cons(4, Nil))))) == Cons(2, Cons(3, Cons(4, Nil))))
  }

  "setHead" should "change the first element of the list" in {
    assert(List.setHead(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), 5) == Cons(5, Cons(2, Cons(3, Cons(4, Nil)))))
  }

  "drop" should "remove n elements from the beginning of the list" in {
    assert(List.drop(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), 2) == Cons(3, Cons(4, Nil)))
  }

  "dropWhile" should "remove elements from the beginning of the list while the condition is true" in {
    assert(List.dropWhile(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), (a: Int) => a < 3) == Cons(3, Cons(4, Nil)))
  }

  "init" should "remove the last element of the list" in {
    assert(List.init(Cons(1, Cons(2, Cons(3, Cons(4, Nil))))) == Cons(1, Cons(2, Cons(3, Nil))))
  }

  "length" should "count elements in a list" in {
    val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    assert(4 == List.length(l))
    assert(4 == List.length2(l))
    assert(4 == List.length3(l))
  }

  "sum3" should "sum the elements of a list" in {
    val l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    assert(10 == List.sum3(l))
  }

  "product3" should "multiply the elements of a list" in {
    val l: List[Double] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    assert(24 == List.product3(l))
  }

  "reverse" should "reverse the elements of a list" in {
    val l: List[Double] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    assert(Cons(4, Cons(3, Cons(2, Cons(1, Nil)))) == List.reverse(l))
  }

  "append" should "append the elements of a list" in {
    val l: List[Double] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val k: List[Double] = Cons(5, Cons(6, Nil))
    val expected = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))
    assert(expected == List.append2(l, k))
  }

  "add1" should "adds 1 to the elements of a list" in {
    val l: List[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val expected = Cons(2, Cons(3, Cons(4, Cons(5, Nil))))
    assert(expected == List.add1(l))
  }

  "toString" should "convert to string the elements of a list" in {
    val l: List[Double] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val expected: List[String] = Cons("1.0", Cons("2.0", Cons("3.0", Cons("4.0", Nil))))
    assert(expected == List.toString(l))
  }

  "map" should "transform the elements of a list" in {
    val l: List[Double] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val expected = Cons(2, Cons(3, Cons(4, Cons(5, Nil))))
    assert(expected == List.map(l)(_ + 1))
  }

  "filter" should "remove elements that don't pass criteria" in {
    val l: List[Double] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val expected = Cons(2, Cons(4, Nil))
    assert(expected == List.filter(l)(_ % 2 == 0))
    assert(expected == List.filter2(l)(_ % 2 == 0))
  }

  "flatMap" should "map and flatten the element of a list" in {
    val l: List[Double] = Cons(1, Cons(2, Cons(3, Nil)))
    val expected = Cons(1, Cons(1, Cons(2, Cons(2, Cons(3, Cons(3, Nil))))))
    assert(expected == List.flatMap(l)(i => Cons(i, Cons(i, Nil))))
  }

  "addList" should "add corresponding elements from two lists" in {
    val l: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))
    val expected: List[Int] = Cons(2, Cons(4, Cons(6, Nil)))
    assert(expected == List.addLists(l, l))
  }

  "addList" should "add corresponding elements from two lists with different sizes" in {
    val l: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))
    val k: List[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    val expected: List[Int] = Cons(2, Cons(4, Cons(6, Cons(4, Nil))))
    assert(expected == List.addLists(l, k))
  }

  "concat" should "concat lists" in {
    val expected: List[List[Int]] = List(List(1, 2, 3), List(4, 5, 6))
    assert(List(1, 2, 3, 4, 5, 6) == List.concat(expected))
  }

  "hasSubsequence" should "check if a list is subsequence of another list" in {
    val l: List[String] = Cons("R", Cons("a", Cons("f", Cons("a", Cons("e", Cons("l", Nil))))))
    val s1: List[String] = Cons("f", Cons("a", Cons("e", Nil)))
    val s2: List[String] = Cons("f", Cons("a", Cons("r", Nil)))
    val s3: List[String] = Cons("e", Cons("l", Nil))
    assert(true == List.hasSubsequence(l, s1))
    assert(false == List.hasSubsequence(l, s2))
    assert(true == List.hasSubsequence(l, s3))
  }

}
