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
    case Cons(a:A, as) => a() :: as().toList/*scala.List[A](a(), as().toList)*/
    case _  => List(): List[A]
  }
  /*def toList_fold: List[A] = {
    this.foldRight(List(): List[A])(( a: A, b: => List[A]) => a :: b )
  }*/

  def take(n: Int): Stream[A] = {
    def helper(k: Int, original: => Stream[A]): Stream[A] = original match{
      case Cons(h,e) => Cons(h, () => helper(k-1, e()))
      case _ => Stream(): Stream[A]
    }
    helper(n, this)
  }

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

  def map[B](f: A => B): Stream[B] = {
    def f_(a: A, b: => Stream[B]): Stream[B]= {
      Cons( () => f(a), () => b)
    }
    this.foldRight(Empty: Stream[B])(f_)
  }

  def filter(p: A => Boolean): Stream[A] = {
    def help(a:A, b: =>Stream[A]): Stream[A] = {
      if (p(a)) Cons(()=>a, ()=>b)
      else b
    }
    this.foldRight(Empty: Stream[A])(help)
  }
  /* Is it strict or not? */
  def append[B>:A](a: => Stream[B]): Stream[B] = {
    def help(c: B, b: => Stream[B]): Stream[B] = {
      Cons(()=> c, () => b)
    }
    this.foldRight(a: Stream[B])(help)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    def help(a:A, b: => Stream[B]): Stream[B] = {
      f(a).append(b)
    }
    this.foldRight(Empty: Stream[B])(help)
  }

  /* ------------ Unfold ------------- */

  def mapUnfold[B](f: A => B): Stream[B] = {
    def helper(a: Stream[A]): Option[(B, Stream[A])] = a match{
      case Cons(ah, at) => Some((f(ah()), at()))
      case Empty => None: Option[(B,Stream[A])]
    }
    unfold(this)(helper)
  }

  /* Test */
  def takeUnfold_bad(n: Int): Stream[A] = this {
    //    def f(k: Int, as: Stream[A]): Option[(A, (Int, Stream[A]))] = as match {
    //      case Cons(ah, at) => if (k<=n) Some((ah():A, (k+1, at()))): Option[(A, (Int, Stream[A]))] else None: Option[(A, (Int, Stream[A]))]
    //      case Empty => None: Option[(A, (Int, Stream[A]))]
    //    }
    def f(kas: Tuple2[Int,Stream[A]]): Option[(A, (Int, Stream[A]))] = kas._2 match {
      case Cons(ah, at) => if (kas._1<=n) Some((ah():A, (kas._1+1, at()))): Option[(A, (Int, Stream[A]))] else None: Option[(A, (Int, Stream[A]))]
      case Empty => None: Option[(A, (Int, Stream[A]))]
    }
    unfold(1, this)(f)
  }

  /* Test */
  def zipWith_bad[B,C](a: Stream[A], b: Stream[B])(f: (A,B) => C): Stream[C] = {
    def h(ab: Tuple2[Stream[A], Stream[B]]): Option[(C,(Stream[A],Stream[B]))] = a match {
      case Cons(ah, at) => b match {
        case Cons(bh, bt) => Some((f(ah(), bh()), (at(), bt())))
        case Empty => None
      }
      case Empty => None
    }
    unfold((a,b))(h)
  }

  def zipAll_bad[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {

    def fb[C,D](ao:Option[C], as:Stream[C])(bs: Stream[D]): Option[((Stream[C], Stream[D]), (Option[C], Option[D]))] = bs match{
      case Cons(bh, bt) => Some(((as, bt()),(ao, Some(bh()))))
      case Empty => ao match {
        case Some(_) => Some(((as, Empty),(ao, None)))
        case None => None}
    }

    def f(ab: Tuple2[Stream[A], Stream[B]]): Option[((Stream[A], Stream[B]),(Option[A], Option[B]))] = ab._1 match {
      case Cons(ah, at) => fb(Some(ah()), at())(ab._2)
      case Empty => fb(None, Empty)(ab._2)
      }
    unfold((this, s2))(f)
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

  def constant[A](a:A) : Stream[A] = {
    Cons(() => a, () => constant(a))
    lazy val l: Stream[A] = Cons(() => a, () => l)
    l
  }

  def from(n: Int): Stream[Int] = {
    Cons(() => n, () => from(n+1))
  }

  def fib(): Stream[Int] = {
    /*unfold((0, 1))((l :Tuple2(Int)) => Some((, (l, k+l))))*/
    unfold((0,1))((l :Tuple2[Int,Int]) => Some((l._2+l._1, (l._2, l._2+l._1))))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    lazy val l: Option[(A,S)] = f(z)
    /*l.map((a:A,s:S) => cons(a, unfold(s)(f))).getOrElse(empty: Stream[A])*/
    l.map((t:Tuple2[A,S]) => cons(t._1, unfold(t._2)(f))).getOrElse(empty: Stream[A])
  }

}