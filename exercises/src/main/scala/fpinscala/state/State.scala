package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    (if (n==Int.MinValue) 0 else if (n<0) (-1)*n else n, rng2)
  }

  /*TEST - nah*/
  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    (n.toDouble/(Int.MaxValue+1), rng2)
  }

  /*TEST - nah*/
  def doubleWithMap(rng: Rand[Int]): Rand[Double] = {
    map(nonNegativeInt)(n => n.toDouble/(Int.MaxValue+1))
  }

  /*TEST - nah*/
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    val (n2, rng3) = double(rng2)
    ((n,n2), rng3)
  }

  /*TEST - nah*/
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((n, n2), rng2) = intDouble(rng)
    ((n2, n), rng2)
  }

  /*TEST - nah*/
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1,d2,d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def inter(count1: Int, rng1: RNG, currentList: List[Int]): (List[Int], RNG) = count1 match {
      case 0 => (currentList, rng1)
      case _ => {
        val (n, rng2) = rng1.nextInt
        inter(count1-1, rng2, n::currentList) //List.concat(List(n),currentList))
      }
    }
    val (l, rngFinal) = inter(count, rng, List())
    (l.reverse, rngFinal)
  }


  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rnga) = ra(rng)
      val (b, rngb) = rb(rnga)
      (f(a,b), rngb)
    }
  }

  /* Implement using foldRight? */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match{
    case Nil => (rng => (List(), rng))
    case t :: xs => map2(t, sequence(xs))( (a,l) => List.concat(List(a), l))
  }

  def sequenceFoldRight[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(RNG.unit(List():List[A]))( (t, r) => map2(t, r)( (a,l) => a::l) )
  }

  /*TEST*/
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => RNG.unit(f(a)))
  }

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(rb)(b => flatMap(ra)(a => RNG.unit(f(a,b))))
  }

  def nonNegativeLessThan(n :Int): Rand[Int] = {
    val f = (i:Int) => { rng: RNG =>
    {
      val mod = i % n
      if (i+ (n-1) - mod >= 0) (mod,rng) else nonNegativeLessThan(n)(rng)}}
    flatMap(RNG.int)(f)
  }

}
import State.unit

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    flatMap(a => unit(f(a)))
    //flatMap(a => State(s => (f(a),s)))
    // Replace State(s => (f(a),s)) with State.unit(f(a))
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sb.flatMap(b => this.flatMap(a => unit(f(a,b))))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State( s => {
      val (a:A , rs:S) = run(s)
      f(a).run(rs)
    })

  //def unit[A](a: A): State[S, A] = State(s => (a,s))

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  //def unlock: Machine = Machine(true, candies, coins)
}

object State {

  type Rand[A] = State[RNG, A]

  def sequence[S,A](fs: List[State[S,A]]): State[S, List[A]] = {
    fs.foldRight( unit(List()): State[S,List[A]] )( (t, r) => t.map2(r)( (a,l) => a ::l ) )
  }

  def unit[S, A](a: A): State[S, A] = State(s => (a,s))

  def get[S]: State[S,S] = State(s => (s,s))

  def set[S](s:S): State[S,Unit] = State(_ => ((),s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def update(input: Input, state: State[Machine, (Int,Int)]): State[Machine, (Int,Int)] = input match {
    case Coin => insertCoin(state)
    case turnKnob => turnKnob(state)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    inputs.foldRight(State(m => ((0,0), m:Machine)))((i, s) => update(i, s))
  }

  /*def insertCoin(state: State[Machine, (Int,Int)]): State[Machine, (Int,Int)] = this match{
    case Machine(true, ca, co) => actIfCandiesLeft( m => Machine(false, ca, co+1))
    case Machine(false, _, _) => state
  }

  def turnKnob(state: State[Machine, (Int,Int)]): State[Machine, (Int,Int)] = this match {
    case Machine(false, ca, co) => actIfCandiesLeft(m => Machine(true, ca-1, co))
    case Machine(true, _, _) => state
  }*/

  def actIfCandiesLeft(state: State[Machine, (Int,Int)])(f: State[Machine, (Int,Int)] => State[Machine, (Int,Int)]) : State[Machine, (Int,Int)] = {
  }

  def stateFromMachine: State[Machine, (Int, Int)] = {
    State(s => ((s.coins, s.candies), s))
  }
}
