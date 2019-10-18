package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Boolean
  def &&(p: Prop): Prop = {
    // New Prop, with a good check
    val a = this
    new Prop {
      def check: Boolean = {
        // Which this does this this refer to in ' this.check & p.check
        a.check & p.check
      }
    }
  }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
}

/*case class Gen[A](sample: State[RNG,A]) {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val s = State(s:RNG => (0,s): [Int, RNG])
    s.map(a => ( a%(stopExclusive -start)) + start)
    /*sample.flatMap( a => State[RNG,Int]( state0 => RNG.map(RNG.nonNegativeLessThan(stopExclusive - start))( int0 => int0 + start))*/
    /*sample.map( a => ( a%(stopExclusive -start)) + start)*/
    /*State.unit(RNG.map(RNG.nonNegativeLessThan(stopExclusive - start))(int0 => int0 + start))*/

  }
}*/

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

