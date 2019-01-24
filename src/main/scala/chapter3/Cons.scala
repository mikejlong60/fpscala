package chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def init[A](l: List[A]): List[A] = {
    def keepCons(l: List[A], ll: List[A]): List[A] = l match {
      case Cons(_, Nil) => ll
      case Cons(h, t) => keepCons(t, append(Cons(h, List()), ll))
      case _ => l
    }

    keepCons(l, apply())
  }
 
  def append[A](a1: List[A], a2: List[A]): List[A] =  a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
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

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  def setHead[A](v: A, as: List[A]): List[A] = (v, as) match {
    case (_, Nil) => List(v)
    case (a,b) => Cons(a, b)
  }

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldRight[A,B](as: List[A], z:B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldRight2[A,B](as: List[A], z:B)(f: (A,B) => B): B = {
    foldLeft(reverse(as), z)((b, a) => f(a, b))
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((x, y) => y + 1)
  
  def length2[A](as: List[A]): Int = foldLeft(as, 0)((x, y) => x + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((z, x) => Cons(x, z))

  def sum3(ns: List[Int]) = foldLeft(ns, 0)((x,y) => y + x)

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

  def sum4(ns: List[Int]) = foldRight2(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

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
