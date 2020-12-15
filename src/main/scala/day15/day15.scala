package themis.aoc2020
package day15

import scala.collection.mutable

object day15 {
  private def isFirstTime(state: mutable.Map[Long, List[Long]], lastSpoken: Long) = state(lastSpoken).length < 2

  //les mutables
  private def addSafe(state: mutable.Map[Long, List[Long]], lastSpoken: Long, turn: Long): Unit = {
    val toAppend = if (!isFirstTime(state, lastSpoken)) state(lastSpoken).tail else state(lastSpoken)
    state.update(lastSpoken, toAppend :+ turn)
  }

  def doTheThing(input: List[Long], howManyTimes: Long): Long = {
    val state: collection.mutable.Map[Long, List[Long]] = collection.mutable.Map().withDefaultValue(List.empty)

    input.zipWithIndex.foreach(i => {
      val atTurn = (i._2 + 1).toLong
      val number = i._1
      state += (number -> List(atTurn))
    })

    var turn = input.size + 1
    var lastSpoken = input.last

    while (turn <= howManyTimes) {
      if (isFirstTime(state, lastSpoken)) {
        lastSpoken = 0
        addSafe(state, lastSpoken, turn)
      } else {
        val rounds = state(lastSpoken)
        lastSpoken = rounds.last - rounds.head
        addSafe(state, lastSpoken, turn)
      }
      turn += 1
    }

    lastSpoken
  }

  def main(args: Array[String]): Unit = {

    val inp: List[Long] = List(16, 1, 0, 18, 12, 14, 19)

    //part1
    println(doTheThing(inp, 2020))
    //part2
    println(doTheThing(inp, 30000000))
  }
}
