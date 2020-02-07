package io.github.gvolpe

import cats.implicits._

object Main extends App {

  def take[A](list: List[A], n: Int): List[A] =
    (list, n) match {
      case (Nil, _) | (_, 0) => Nil
      case ((x :: xs), i)    => x :: take(xs, i - 1)
    }

  val list = List.range(1, 11)

  def takeN[A](list: List[A], n: Int): List[A] =
    (list, n) match {
      case (_, 0)                    => Nil
      case (_, i) if (list.size < i) => list ::: takeN(list, i - list.size)
      case ((x :: xs), i)            => x :: takeN(xs, i - 1)
    }

  assert(take(list, 200) === list.take(200))
  assert(takeN(list, 50) === (list ::: list ::: list ::: list ::: list))
  assert(takeN(list, 15) === (list ::: list.take(5)))

  def reverse[A](list: List[A]): List[A] =
    list match {
      case Nil       => Nil
      case (x :: xs) => reverse(xs) ::: List(x)
    }

  assert(reverse(list) === list.reverse)

  def drop[A](list: List[A], n: Int): List[A] =
    (list, n) match {
      case (Nil, _)       => Nil
      case (xs, 0)        => xs
      case ((_ :: xs), i) => drop(xs, i - 1)
    }

  def dropUsingTakeAndReverse[A](list: List[A], n: Int): List[A] =
    if (list.size >= n) reverse(take(reverse(list), list.size - n))
    else Nil

  assert(drop(list, 3) === list.drop(3))
  assert(drop(list, 17) === list.drop(17))

  def filter[A](list: List[A], f: A => Boolean): List[A] =
    list match {
      case Nil                 => Nil
      case (x :: xs) if (f(x)) => x :: filter(xs, f)
      case (_ :: xs)           => filter(xs, f)
    }

  assert(filter(list, (n: Int) => n == 10) === list.filter(_ == 10))

  val isEven: Int => Boolean = n => (n % 2) == 0
  val isOdd: Int => Boolean  = n => !isEven(n)

  def evens(list: List[Int]): List[Int] =
    filter(list, isEven)

  def odds(list: List[Int]): List[Int] =
    filter(list, isOdd)

  assert(odds(list) === List(1, 3, 5, 7, 9))
  assert(evens(list) === List(2, 4, 6, 8, 10))

  val foo       = List(5, 3, 4, 7, 2, 1, 5, 6)
  val sortedFoo = foo.sorted

  assert(Sort.quicksort(foo) === sortedFoo)
  assert(Sort.mergesort(foo) === sortedFoo)
  assert(Sort.insertsort(foo) === sortedFoo)

}
