package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match{
    case Cons(a:A, as: Stream[A]) => a() :: as().toList/*scala.List[A](a(), as().toList)*/
    case _  => List(): List[A]
  }
  /*def toList_fold: List[A] = {
    this.foldRight(List(): List[A])(( a: A, b: => List[A]) => a :: b )
  }*/

  /*def take(n: Int): Stream[A] = {
    def helper(k: Int, original: Stream[A]): Stream[A] = original match{
      case Cons(h,e) => Stream(h, helper(k-1, e()))
      case _ => Stream(): Stream[A]
    }
    helper(n, this)
  }*/

  def drop(n: Int): Stream[A] = {
    def helper(k: Int, s: Stream[A]): Stream[A] = k match {
      case 0 => s
      case _ => s match {
        case Cons(a, l) => helper(k-1,l())
        case Empty => Empty:Stream[A]
      }
    }
    helper(n, this)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    /*def helper(taken: Stream[A], feed: Stream[A]): Stream[A] = feed match{
      case Cons(a,l) => if (p(a)) helper(Cons(a,taken()), l()) else taken
      case Empty => taken
    }*/
    def helper(feed: Stream[A]): Stream[A] = feed match{
      case Cons(a, l) => {
        if (p(a())) {
          Cons(a, () => helper( l() ) )
        }
        else {
          Empty:Stream[A]
        }
      }
      case Empty => Empty:Stream[A]
    }
    helper(this)
  }

  def takeWhile_fold(p: A => Boolean): Stream[A] = {
    /* If the first element satisfies the condition, return the stream.
    Otherwise, return an empty one. */
    def help(a:A, b: =>Stream[A]): Stream[A] = {
      if (p(a)) Cons(()=>a,()=>b)
      else Empty: Stream[A]
    }
    this.foldRight(Stream():Stream[A])(help)
  }

  /*Question: Why don't we write (...&& l() )? According to the definition in foldRight, l should be evaluated.*/
  def forAll(p: A => Boolean): Boolean = {
    this.foldRight( true )( (a: A, l) => ( p(a) && l ))
  }

  def headOption: Option[A] = {
    def helper(a: A, b: =>Option[A]):Option[A] = {
      Some(a)
    }
    this.foldRight(None:Option[A])(helper)
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  /*TO TEST*/
  def map[B](f: A => B): Stream[B] = {
    def f_(a: A, b: => Stream[B]): Stream[B]= {
      Cons( () => f(a), () => b)
    }
    this.foldRight(Empty: Stream[B])(f_)
  }

  /*TO TEST*/
  def filter(p: A => Boolean): Stream[A] = {
    def help(a:A, b: =>Stream[A]): Stream[A] = {
      if (p(a)) Cons(()=>a, ()=>b)
      else b
    }
    this.foldRight(Empty: Stream[A])(help)
  }
  /* TEST, Is it strict or not? */
  def append[B>:A](a: => Stream[B]): Stream[B] = {
    def help(c: B, b: => Stream[B]): Stream[B] = {
      Cons(() => c, () => b)
    }
    this.foldRight(a: Stream[B])(help)
  }
  /* TEST */
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    def help(a:A, b: => Stream[B]): Stream[B] = {
      f(a).append(b)
    }
    this.foldRight(Empty: Stream[B])(help)
  }

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}