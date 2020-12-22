package themis.aoc2020
package day22

import scala.io.Source
import scala.util.Using

object day22 {
  type Deck = List[Long]

  def doTheThing1(p1deck: Deck, p2deck: Deck): Unit = {
    var p1 = p1deck
    var p2 = p2deck

    var round = 0
    while (p1.nonEmpty && p2.nonEmpty) {
      val p1plays = p1.head
      p1 = p1.tail
      val p2plays = p2.head
      p2 = p2.tail

      if (p2plays > p1plays) {
        p2 = p2 ::: List[Long](p2plays, p1plays)
      } else {
        p1 = p1 ::: List[Long](p1plays, p2plays)
      }
      round += 1
    }
    val winner = if (p1.nonEmpty) p1 else p2
    println(winner.zipWithIndex.map(x => x._1 * (winner.size - x._2)).sum)
  }


  def doTheThing2(p1deck: Deck, p2deck: Deck): (Int, Deck) = {
    var p1 = p1deck
    var p2 = p2deck

    val history1: collection.mutable.Set[Deck] = collection.mutable.Set()
    val history2: collection.mutable.Set[Deck] = collection.mutable.Set()

    while ((p1.nonEmpty && p2.nonEmpty)) {

      if ((history1 contains p1) && (history2 contains p2)) {
        return (1, p1)
      }

      history1.add(p1)
      history2.add(p2)

      val p1plays = p1.head
      p1 = p1.tail
      val p2plays = p2.head
      p2 = p2.tail


      if (p2plays <= p2.size && p1plays <= p1.size) {
        val winner = doTheThing2(p1.take(p1plays.toInt), p2.take(p2plays.toInt))
        if (winner._1 == 1) {
          p1 = p1 ::: List[Long](p1plays, p2plays)
        } else {
          p2 = p2 ::: List[Long](p2plays, p1plays)
        }
      } else {
        if (p2plays > p1plays) {
          p2 = p2 ::: List[Long](p2plays, p1plays)
        } else {
          p1 = p1 ::: List[Long](p1plays, p2plays)
        }
      }
    }

    if (p1.nonEmpty) (1, p1) else (2, p2)
  }


  def main(args: Array[String]): Unit = {

    val inp: String = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day22.txt")) {
      source =>
        source.mkString
    }.get

    val i = inp.split("\\n\\n")
      .map(s => s.split("\n").tail.toList.map(s => s.trim.toLong))
      .toList

    val p1 = i.head
    val p2 = i.last
    //part1
    doTheThing1(p1, p2)

    //part2
    val winner = doTheThing2(p1, p2)
    println(winner._2.zipWithIndex.map(x => x._1 * (winner._2.size - x._2)).sum)
  }
}
