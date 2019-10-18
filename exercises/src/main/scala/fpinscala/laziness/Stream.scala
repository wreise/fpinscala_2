package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def scanRight[B](z: => B)(f:(A, =>B) => B): Stream[B] = {
    val res = this.foldRight((z, Stream(z)))((a, b) => {
      val v = f(a, b._1)
      (v, cons(v, b._2))
    })
    res._2
    // It is impossible with unfold, because the current state should take information from the state on the right, while in unfold, it takes it from the left.
  }

  /*def scanLeft[S](a:S)(f: (S,A) => S): Stream[S] = {
    def h(sa: (S, Stream[A])): Option[(S,(S,Stream[A]))] = sa._2 match{
      case Cons(ah, at) => Some((sa._1, (f(sa._1, ah()), at())))
      case Empty => None
    }
    unfold((a,this))(h)
  }*/

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h:A,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
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
    case Cons(a, as) => a() :: as().toList/*scala.List[A](a(), as().toList)*/
    case _  => List(): List[A]
  }
  /*def toList_fold: List[A] = {
    this.foldRight(List(): List[A])(( a: A, b: => List[A]) => a :: b )
  }*/

  def take(n: Int): Stream[A] = {
    def helper(k: Int, original: => Stream[A]): Stream[A] = original match{
      case Cons(h,e) => if (k>0) Cons(h, () => helper(k-1, e())) else Empty
      case _ => Empty: Stream[A]
    }
    helper(n, this)
  }
 /* RUN the test*/
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
      if (p(a)) cons(a, b)
      else empty: Stream[A]
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

  /*Tested in take*/
  def takeUnfold(n:Int): Stream[A] = {
    unfold((n, this))(
      (ab) =>  ab._2 match {
        case Cons(a, at) => if (ab._1>0) Some((a(),(ab._1-1, at()))) else None
        case Empty => None
      }
    )
  }

  def zipWith[B,C](b: Stream[B])(f: (A,B) => C): Stream[C] = {
    def h(ab: (Stream[A], Stream[B])): Option[(C,(Stream[A],Stream[B]))] = ab._1 match {
      case Cons(ah, at) => ab._2 match {
        case Cons(bh, bt) => Some((f(ah(), bh()), (at(), bt())))
        case Empty => None
      }
      case Empty => None
    }
    unfold((this,b))(h)
  }

  def zipWithNoUnfold[B,C](b: Stream[B])(f: (A,B) => C): Stream[C] = this match {
    case Cons(ah, at) => b match {
      case Cons(bh, bt) => cons( f(ah(), bh()), at().zipWithNoUnfold(bt())(f))
      case Empty => Empty
    }
    case Empty => Empty
  }

  /*Test*/
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {

    def h(ab: (Stream[A], Stream[B])): Option[((Option[A], Option[B]), (Stream[A], Stream[B]))] = ab match {
      case (Cons(ah, at), Cons(bh,bt)) => Some(((Some(ah()), Some(bh())), (at(),bt())))
      case (Empty, Cons(bh,bt)) => Some(((None, Some(bh())), (Empty,bt())))
      case (Cons(ah, at), Empty) => Some(((Some(ah()), None), (at(),Empty) ))
      case (Empty, Empty) => None
    }
    unfold((this, s2))(h)
  }


  def startsWith[A](s: Stream[A]): Boolean = {
    zipWith(s)((a,b)=> a==b).forAll(b => b)
  }

  def tails: Stream[Stream[A]] = {
    unfold(this)(
      (s: Stream[A]) => s match {
        case Cons(a, at) => Some(s, at())
        case Empty => None
      }
    )
  }
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
    cons( a,  constant(a))
    /*lazy val l: Stream[A] = Cons(() => a, () => l)
    l*/
  }

  def from(n: Int): Stream[Int] = {
    cons( n, from(n+1))
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