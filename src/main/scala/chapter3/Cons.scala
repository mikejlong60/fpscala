package chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def init[A](l: List[A]): List[A] = {
  }
  
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => as
    case Cons(x, xs) => xs
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case x if x <= 0 => l
    case _ => drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def setHead(v: Int, as: List[Int]): List[Int] = (v, as) match {
    case (_, Nil) => List(v)
    case (a,b) => Cons(a, b)
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
