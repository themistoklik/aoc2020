package themis.aoc2020
package day23

import scala.io.Source
import scala.util.Using

object day23 {

  private def getDestinationCup(currentCup: Node) = {
    if ((currentCup.id - 1) == 0) 1_000_000 else currentCup.id - 1
  }

  private def getDestinationCup(id: Int): Int = {
    if ((id - 1) == 0) 1_000_000 else id - 1
  }

  type Label = Char

  type Index = Int

  class Node(val id: Int, var next: Node = null)

  def decrementCup(cup: Char, minCup: Int, maxCup: Int): Int = {
    var destCup: Int = cup.asDigit - 1
    if (destCup < minCup) {
      destCup = maxCup
    }
    destCup
  }

  def getDestCup(currCup: Char, inp: String, picks: List[Label]): Char = {
    val minCup = inp.toList.map(i => i.asDigit).min
    val maxCup = inp.toList.map(i => i.asDigit).max

    var destCup: Int = decrementCup(currCup, minCup, maxCup)

    while (picks contains (destCup + 48).toChar) {
      destCup = decrementCup((destCup + 48).toChar, minCup, maxCup)
    }
    (destCup + 48).toChar
  }

  def main(args: Array[String]): Unit = {

    val input: String = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day23.txt")) {
      source =>
        source.mkString
    }
      .get

    //part1
    var inp = input

    var currIdx = 0
    1.to(100).foreach(_ => {
      val currCup = inp(currIdx)

      val nexts: List[Index] = 1.to(3).map(i => (currIdx + i) % inp.length).toList

      val pickUp: List[Label] = List(nexts.map(idx => inp(idx)): _*)

      val destCup = getDestCup(currCup, inp, pickUp)

      val p = (inp ++ inp).slice(currIdx + 1, currIdx + 4)
      inp = inp.filterNot(c => pickUp contains c)
      val enterAt = inp.indexOf(destCup) + 1 % inp.length
      inp = inp.slice(0, enterAt) ++ p ++ inp.slice(enterAt, inp.length)
      currIdx = (inp.indexOf(currCup) + 1) % inp.length
    })

    println(inp.dropWhile(c => c != '1').tail ++ inp.takeWhile(c => c != '1'))

    //part2, well the janky solution can only carry me so far, LETS GO LINKEDLIST
    val cups: collection.mutable.Map[Int, Node] = collection.mutable.Map()

    val inputNums = input.toSeq.map(n => n.asDigit)

    inputNums
      .map(n => new Node(n))
      .foreach(node => cups += node.id -> node)

    inputNums.max to 1_000_000 foreach { n =>
      cups += n -> new Node(n)
    }

    (inputNums ++ Range(10, 1_000_001) ++ Seq(inputNums.head))
      .sliding(2, 1)
      .foreach(pair => {
        val from = pair.head
        val to = pair.last

        cups(from).next = cups(to)
      })

    var currentCup = cups(inputNums.head)
    0 to 10_000_000 foreach { _ =>
      val picks: List[Int] = List(currentCup.next, currentCup.next.next, currentCup.next.next.next)
        .map(node => node.id)
      currentCup.next = cups(picks.last).next

      var destinationCupId = getDestinationCup(currentCup)
      while (picks contains destinationCupId) {
        destinationCupId = getDestinationCup(destinationCupId)
      }
      val destinationCup = cups(destinationCupId)

      cups(picks.last).next = destinationCup.next
      destinationCup.next = cups(picks.head)
      currentCup = currentCup.next

    }

    println(cups(1).next.id.toLong * cups(1).next.next.id.toLong)
  }
}
