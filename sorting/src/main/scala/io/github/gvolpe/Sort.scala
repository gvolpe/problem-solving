package io.github.gvolpe

import cats.Order
import cats.implicits._

object Sort {

  def quicksort[A: Order](list: List[A]): List[A] =
    list match {
      case Nil => Nil
      case (x :: xs) =>
        val ls = xs.filter(_ <= x)
        val rs = xs.filter(_ > x)
        quicksort(ls) ::: List(x) ::: quicksort(rs)
    }

  def mergesort[A: Order](list: List[A]): List[A] = {
    def merge(ls: List[A], rs: List[A]): List[A] =
      (ls, rs) match {
        case (a, Nil)                         => a
        case (Nil, b)                         => b
        case ((x :: xs), (y :: ys)) if x <= y => x :: merge(xs, y :: ys)
        case ((x :: xs), (y :: ys))           => y :: merge(ys, x :: xs)
      }

    list match {
      case Nil        => Nil
      case (x :: Nil) => List(x)
      case xs =>
        val half = xs.size / 2
        val fst  = mergesort(xs.take(half))
        val snd  = mergesort(xs.drop(half))
        merge(fst, snd)
    }
  }

  def insertsort[A: Order](list: List[A]): List[A] = {
    def insert(y: A, ys: List[A]): List[A] =
      (y, ys) match {
        case (a, Nil)                 => List(a)
        case (a, (b :: c)) if (a < b) => a :: b :: c
        case (a, (b :: c))            => b :: insert(a, c)
      }

    list match {
      case Nil       => Nil
      case (x :: xs) => insert(x, (insertsort(xs)))
    }
  }

}
