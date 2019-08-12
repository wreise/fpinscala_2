package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  /* Q: Write it using the get method? */
 def map[B](f: A => B): Either[E, B] = this match {
   case Left(e) => Left(e)
   case Right(a) => Right(f(a))
 }

  /* Apparently, flatMap cannot accumulate errors - why? IS it because the first Either can ealready be a Left, and hence, even if f always evalautes to an error, it will not be traversed?*/
 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Right(a) => f(a)
   case Left(e) => Left(e)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match{
   case Left(_) => b
   case Right(e) => Right(e)
   /*this.flatMap((_)=> b)*/
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
   this.flatMap(a => b.map(b2 => f(a, b2)))
 }

}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  /* Needed to specify explicitly that Right(fpinscala.datastructures.List()) is of type Either[E,fpinscala.datastructures.List[B]] */
  def traverse[E, A, B](es: fpinscala.datastructures.List[A])(f: A => Either[E, B]): Either[E, fpinscala.datastructures.List[B]] = {
    fpinscala.datastructures.List.foldRight(es, Right(fpinscala.datastructures.List()): Either[E,fpinscala.datastructures.List[B]])((a: A, bs: Either[E, fpinscala.datastructures.List[B]]) =>
      /*f(a).map2(bs)((e: Either[E,B], be: Either[E,fpinscala.datastructures.List[B]]) => Right(fpinscala.datastructures.List.append(fpinscala.datastructures.List(e), be))) )*/
    f(a).map2(bs)( (eNew: B, eOldList: fpinscala.datastructures.List[B]) => fpinscala.datastructures.List.append(fpinscala.datastructures.List(eNew), eOldList)))
  }

  def sequence[E,A](es: fpinscala.datastructures.List[Either[E,A]]): Either[E,fpinscala.datastructures.List[A]] ={
    traverse(es: fpinscala.datastructures.List[Either[E,A]])(a => a)
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}