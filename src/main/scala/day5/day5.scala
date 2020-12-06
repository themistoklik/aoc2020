package themis.aoc2020
package day5

import scala.io.Source
import scala.util.Using
import scala.util.control.Breaks.break

object day5 {
  def stringToId(seat: String, lo: String, hi: String, rangeStart: Int, rangeEnd: Int): Long = {
    var s = rangeStart
    var e = rangeEnd
    for (code <- seat) {
      if (s >= e) break
      if (code.toString == lo) {
        e = (s + e) / 2
      } else {
        s = ((s + e) / 2) + 1
      }
    }
    s
  }

  def getRow(seat: String): Long = stringToId(seat, "F", "B", 0, 127)

  def getColumn(seat: String): Long = stringToId(seat, "L", "R", 0, 7)

  def getSeatId(seat: String): Long = getRow(seat.take(7)) * 8 + getColumn(seat.takeRight(3))

  def main(args: Array[String]): Unit = {
    val lines = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day5.txt")) {
      source => source.getLines().toList
    }

    val sortedIds = lines.get.map(getSeatId).sorted
    println("part1")
    println(lines.get.map(getSeatId).max)

    for (id <- 1 until sortedIds.size - 1) {
      if (sortedIds(id) - sortedIds(id - 1) != 1) {
        println("part2")
        println(sortedIds(id)-1)

      }
    }
  }
}
