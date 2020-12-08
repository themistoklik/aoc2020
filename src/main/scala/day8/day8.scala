package themis.aoc2020
package day8

import scala.collection.mutable
import scala.io.Source
import scala.util.{Try, Using}
import scala.util.control.Breaks.{break, breakable}

object day8 {
  def main(args: Array[String]): Unit = {
    //part1
    val nodes = getInput
    val candidates: mutable.Stack[(Int, List[String])] = runProgram(nodes)

    //part2
    candidates.foreach(e => flipOpsAndRunProgram(e))
  }

  private def flipOpsAndRunProgram(e: (Int, List[String])) = {

    var ns = getInput
    if (e._2.head == "jmp") {
      ns = ns.updated(e._1, List("nop", "1"))
    } else {
      ns = ns.updated(e._1, List("jmp", e._2.last))
    }
    runProgram(ns)

  }

  private def getInput = {
    val nodes = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day8.txt")) {
      source =>
        source.getLines()
          .zipWithIndex
          .map(line => {
            line._2 -> line._1.split(" ").toList
          })
          .toMap
    }
    nodes.get
  }

  private def runProgram(ns: Map[Int, List[String]]): mutable.Stack[(Int,List[String])] = {
    val visited: mutable.Set[Int] = mutable.Set()
    var acc = 0

    val stack: mutable.Stack[(Int, List[String])] = mutable.Stack()
    breakable {
      var index = 0
      while (index < ns.size) {
        if (visited contains index) {
          println(acc)
          println(stack)
          break
        }
        if(index==630) {
          println("END")
          println(acc,index)
          break
        }
        visited.add(index)
        ns(index).head match {
          case "acc" =>
            acc += ns(index).last.toInt
            index += 1
          case "jmp" =>
            stack push index -> ns(index)
            index += ns(index).last.toInt
          case "nop" =>
            stack push index -> ns(index)
            index += 1
        }
      }
    }
    stack
  }
}
