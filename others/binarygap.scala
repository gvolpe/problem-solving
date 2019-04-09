object Main {
  def main(args: Array[String]): Unit =
    List(1041,529,20,15,32,5,9).foreach(n => Solution.solution(n))
}

// https://app.codility.com/demo/results/training3AJ3CR-ADZ/
object Solution {
  def solution(n: Int): Int = {
    def gaps(b: List[Char], max: Int): Int =
      b match {
        case Nil => 0
        case ('1'::xs) => gaps(xs, max)
        case xs =>
          val ys = xs.takeWhile(_ != '1')
          val zs = xs.dropWhile(_ != '1')
          if ((zs.length > 0) && zs.contains('0')) gaps(zs, if (ys.length < max) max else ys.length)
          else if ((zs.isEmpty) || (ys.length < max)) max
          else ys.length
      }

    val result = gaps(n.toBinaryString.toList, 0)

    println(s"Solution for $n = $result")

    result
  }
}
