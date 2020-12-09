package themis.aoc2020
package day9

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object day9 {

  def canBeMadeByPreamble(preamble: Set[Long], n: Long): Boolean = {
    preamble
      .map(p => preamble contains math.abs(n - p))
      .exists(p => p)
  }

  def main(args: Array[String]): Unit = {
    //part1
    val numbers = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day9.txt")) {
      source =>
        source.getLines()
          .toList
          .map(n => n.toLong)
    }

    //part1
    val rest = numbers.get
      .zipWithIndex
      .drop(25)
      .map(n => n._1 -> canBeMadeByPreamble(numbers.get.slice(n._2 - 25, n._2).toSet, n._1))
      .find(pair => !pair._2)
      .get

    println(rest)

    //part2
    val target = rest._1
    val sums: mutable.Map[Long, List[Int]] = mutable.Map()

    sums.update(0, List(0))

    var curr: Long = 0
    for (i <- numbers.get.indices) {
      curr += numbers.get(i)

      if (sums.keySet contains curr - target) {
        sums.update(curr - target, sums(curr - target).appended(i))
      }
      sums.put(curr, List(i))
    }

    val boundaries = sums.maxBy(p => p._2.last - p._2.head)._2
    val lo = boundaries.head + 1
    val hi = boundaries.last
    val seq = numbers.get.slice(lo, hi + 1)
    println(seq)
    println(seq.sum)
    print(seq.max + seq.min)

  }
}
