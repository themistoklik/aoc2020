package themis.aoc2020
package day10

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object day10 {
  def main(args: Array[String]): Unit = {

    //part1
    val numbers = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day10.txt")) {
      source =>
        source.getLines()
          .toList
          .map(n => n.toInt)
    }

    val startJoltage = 0
    val joltDiffs = mutable.Map(1 -> 0, 2 -> 0, 3 -> 0)
    val adapters = numbers
      .get
      .sorted

    val deviceJoltage = adapters.max + 3
    joltDiffs.update(adapters.head - startJoltage, joltDiffs(adapters.head - startJoltage) + 1)

    var ns: Unit = adapters
      .sliding(2)
      .foreach(pair => {
        val diff = pair.last - pair.head
        joltDiffs.update(diff, joltDiffs(diff) + 1)
      })

    joltDiffs.update(deviceJoltage - adapters.last, joltDiffs(deviceJoltage - adapters.last) + 1)
    println(joltDiffs(1) * joltDiffs(3))

    //part2
    val jolts = List(startJoltage) ++ adapters ++ List(deviceJoltage)
    val joltPaths = collection.mutable.Map[Long, Long]()
    jolts
      .map(x => x -> 0)
      .foreach(p => joltPaths.update(p._1, p._2))

    joltPaths.update(startJoltage, 1)

    jolts
      .zipWithIndex
      .foreach(j => {

        val index = j._2
        val actualValue = j._1

        val compatibleJumps = lookAhead3(jolts, index)
          .filter(x => x - actualValue <= 3)

        //think of it not as DP but like voting. Each jolt "upvotes" the ones it can reach by the number of ways itself can be reached
        compatibleJumps
          .foreach(x =>
            joltPaths.update(x, joltPaths(x) + joltPaths(actualValue)))
      }
      )
    println(joltPaths(deviceJoltage))
  }

  private def lookAhead3(jolts: List[Int], index: Int) = jolts.slice(index + 1, index + 4)
}
