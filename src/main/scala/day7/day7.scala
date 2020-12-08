package themis.aoc2020
package day7

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object day7 {
  def pathExists(target: String, nodes: Map[String, List[(String, String)]], start: String): Int = {
    val visited: mutable.Set[String] = mutable.Set()
    val q: mutable.Queue[String] = mutable.Queue(start)
    var canReach = 0

    while (q.nonEmpty) {
      val currNode = q.dequeue()
      if (currNode == target) canReach = 1
      val neighbors = nodes(currNode).map(node => node._2)
      if (neighbors.forall(n => !visited.contains(n))) {
        q.enqueueAll(neighbors)
      }
    }
    canReach
  }

  def getBags(start: (String, String), nodes: Map[String, List[(String, String)]], acc: Int): Int = {
    val neighbors = nodes(start._2)
    var res = acc
    for (n <- neighbors) {
      res += n._1.toInt + n._1.toInt * getBags(n, nodes, acc)
    }
    res
  }

  def getMapEntry(rule: String): (String, List[(String, String)]) = {
    val split = rule.split("contain").toList
    val color = split.head.replace("bags", "").trim
    if (split.last contains "no other bags") {
      Tuple2(color, List())
    } else {
      val entries = split.last
        .replace("bags", "")
        .replace("bag", "")
        .replace(".", "")
        .split(",")
        .toList
        .map(e => e.trim.split(" ", 2).toList) //1+ pale teal bag|s
        .map(splitted => Tuple2(splitted.head, splitted.last))
      Tuple2(color, entries)
    }
  }

  def main(args: Array[String]): Unit = {
    val target = "shiny gold"
    val nodes = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day7.txt")) {
      source =>
        source.getLines()
          .toList
          .map(getMapEntry)
          .toMap
    }
    //part1
    println(nodes.get.keys
      .filter(k => k != target)
      .toList
      .map(x => pathExists(target, nodes.get, x)).sum)

    //part2
    println(getBags(("2", target), nodes.get, 0))
  }
}
