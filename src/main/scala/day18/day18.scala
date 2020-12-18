package themis.aoc2020
package day18


import scala.io.Source
import scala.util.Using

object day18 {
  def add(a: Long, b: Long): Long = a + b

  def mul(a: Long, b: Long): Long = a * b

  def min(a: Long, b: Long): Long = a - b

  def div(a: Long, b: Long): Long = a / b

  def evalExpr2(e: List[String], i: Int = 0): (Int, Long) = {


    val nums: collection.mutable.Stack[Long] = collection.mutable.Stack()
    val ops: collection.mutable.Stack[String] = collection.mutable.Stack()
    val operations: Map[String, (Long, Long) => Long] = Map("/" -> div, "+" -> add, "*" -> mul, "-" -> min)

    var index = i
    while (index < e.size) {
      val x: String = e(index)
      if (operations.keySet contains x) {
        while (ops.nonEmpty && ops.head == "+" && x != "+") {
          nums.push(operations(ops.pop())(nums.pop(), nums.pop()))
        }
        ops.push(x)
      } else if (x == "(") {
        val r = evalExpr2(e, index + 1)
        index = r._1
        nums.push(r._2)
      } else if (x == ")") {
        while (ops.nonEmpty) {
          nums.push(operations(ops.pop())(nums.pop(), nums.pop()))
        }
        return (index, nums.pop())
      } else {
        nums.push(x.toLong)
      }
      index += 1
    }
    while (ops.nonEmpty) {
      nums.push(operations(ops.pop())(nums.pop(), nums.pop()))
    }
    (index, nums.pop())
  }

  def evalExpr(e: List[String], i: Int = 0): (Int, Long) = {

    val nums: collection.mutable.Stack[Long] = collection.mutable.Stack()
    val ops: collection.mutable.Stack[String] = collection.mutable.Stack()
    val operations: Map[String, (Long, Long) => Long] = Map("/" -> div, "+" -> add, "*" -> mul, "-" -> min)
    var index = i
    while (index < e.size) {
      val x: String = e(index)
      if (operations.keySet contains x) {
        ops.push(x)
      } else if (x == "(") {
        val r = evalExpr(e, index + 1)
        index = r._1
        nums.push(r._2)
      } else if (x == ")") {
        return (index, nums.pop())
      } else {
        nums.push(x.toLong)
      }
      index += 1
      if (nums.size == 2) {
        nums.push(operations(ops.pop())(nums.pop(), nums.pop()))
      }
    }
    (index, nums.pop())
  }

  def main(args: Array[String]): Unit = {

    val inp = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day18.txt")) {
      source =>
        source
          .getLines()
          .map(l => l.replaceAll(" ", ""))
          .toList
    }
      .get

    //part1
    println(inp.map(i => evalExpr("\\d+|[-+*/()]".r.findAllIn(i).toList)._2).sum)
    //part2
    println(inp.map(i => evalExpr2("\\d+|[-+*/()]".r.findAllIn(i).toList)._2).sum)
  }
}
