package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // x will be 3
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(x, t) => Cons(h, t)
  }


  /** This function takes constant time because we always have a pointer to the
    * beginning of the list. It is just a matter of changing that pointer to point
    * to the second element.
    */
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => l
    case Cons(h, t) => if (n == 1) t else drop(t, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(h, t) => if (!f(h)) Cons(h, t) else dropWhile(t, f)
  }

  /**
    * This takes time proportional to the size of the list. There's no way to remove the last
    * element without traversing the list all the way to the last element.
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  /**
    * Linear in time and space
    */
  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(h, t) => 1 + length(t)
  }

  /** Linear in time, constant in space */
  def length2[A](l: List[A]): Int = {
    @tailrec
    def go(a: List[A], acc: Int): Int = a match {
      case Nil => acc
      case Cons(h, t) => go(t, acc + 1)
    }

    go(l, 0)
  }

  def length3[A](l: List[A]): Int = foldLeft(l, 0)((b, a) => 1 + b)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))


  /*
    fl((1, 2, 3, 4), 0)((b, a) => a + b)
    fl((2, 3, 4), 1 + 0)((b, a) => a + b)
    fl((3, 4), 2 + 1 + 0)((b, a) => a + b)
      ...

   */
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  def append2[A](as: List[A], bs: List[A]): List[A] =
    foldRight(as, bs)((a, b) => Cons(a, b))

  /*
    append2(l1, append2(l2, append2(l3, Nil)))
   */
  def concat[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])((a, b) => append2(a, b))

  def add1(as: List[Int]): List[Int] = foldRight(as, Nil: List[Int])((a, b) => Cons(a + 1, b))

  def toString(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String])((a, b) => Cons(a.toString, b))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A, B](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  def filter2[A, B](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) Cons(a, Nil) else Nil)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil: List[B])((a, b) => append2(f(a), b))

  def flatMap2[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def addLists(as: List[Int], bs: List[Int]): List[Int] = as match {
    case Nil => bs
    case Cons(ha, ta) => bs match {
      case Nil => as
      case Cons(hb, tb) => Cons(ha + hb, addLists(ta, tb))
    }
  }

  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = as match {
    case Nil => bs
    case Cons(ha, ta) => bs match {
      case Nil => as
      case Cons(hb, tb) => Cons(f(ha, hb), zipWith(ta, tb)(f))
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def loop(l: List[A], remaining: List[A]): Boolean = remaining match {
      case Nil => true
      case Cons(h, t) => l match {
        case Nil => false
        case Cons(x, y) => if(h == x) loop(y, t) else loop(y, sub)
      }
    }
    loop(sup, sub)
  }


}
