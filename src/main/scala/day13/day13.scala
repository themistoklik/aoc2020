package themis.aoc2020
package day13

import scala.io.Source
import scala.util.Using

object day13 {
  def main(args: Array[String]): Unit = {

    val instructions = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day13.txt")) {
      source =>
        source.getLines()
          .toList
    }
      .get


    val departAt = instructions.head.toInt
    val bus = instructions
      .last
      .split(",")
      .filter(b => b != "x")
      .map(b => b.toInt)
      .toList
      .map(b => (((departAt / b) + 1) * b - departAt, b))
      .minBy(bb => bb._1)

    println(departAt)
    println(bus)
    val waiting: Long = bus._1
    //part1
    println(waiting * bus._2)

    //part2
    val buses = instructions
      .last
      .split(",")
      .zipWithIndex
      .filter(b => b._1 != "x")
      .map(b => (b._1.toInt, b._2))
      .toList

    var os: Long = buses.head._1
    var t = 0L
    //bus,index
    //thanks https://todd.ginsberg.com/post/advent-of-code/2020/day13/, before this I had the same bruteforce
    //that i dumbly just ran manually for pairs of buses to get from a period to another

    //main idea here is that you don't need to search incrementing by the first bus value. You can
    //save a lot of search space by multiplying your step by `bus` since any offset has to be a product of these buses.
    buses.tail.foreach(b => {
      while ((t + b._2) % b._1 != 0L) {
        t += os
      }
      os *= b._1
    })
    println(t)
  }
}