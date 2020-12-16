package themis.aoc2020
package day16

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

object day16 {

  def rangeFrom(s: String): List[Range] = {
    val pattern = """(\d+)-(\d+) or (\d+)-(\d+)""".r
    s.trim match {
      case pattern(rs1, re1, rs2, re2) => List(Range(rs1.toInt, re1.toInt + 1, 1), Range(rs2.toInt, re2.toInt + 1, 1))
      case _ => List()
    }
  }

  def main(args: Array[String]): Unit = {
    val info = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day16.txt")) {
      source =>
        source
          .mkString
          .split("\\n\\n")
    }
      .get

    val rules = info.head.split("\n").map(r => {
      val spl = r.split(":")
      spl.head.trim -> rangeFrom(spl.last)
    })
      .toMap

    val myTicket = info
      .tail
      .head
      .split("\n")
      .last
      .trim
      .split(",")
      .map(i => i.toInt)
      .toList

    val nearbyTickets = info
      .tail
      .last
      .split("\n")
      .tail
      .map(nt => nt.trim.split(",").map(i => i.toInt).toList)
      .toList

    //part1
    println(nearbyTickets
      .flatten
      .filter(n => rules.values.flatten.map(range => range contains n).forall(p => !p))
      .sum
    )

    //part2
    val valids = nearbyTickets
      .filter(ticket =>
        ticket.map(ticketValue => rules.values.flatten.map(range => range contains ticketValue).exists(p => p))
          .forall(p => p))

    val index2TicketFieldName: collection.mutable.Map[Int, Set[String]] = collection.mutable.Map()

    //start assuming that an index of our ticket can be all ticket fields
    //we'll trim this down to an answer later
    0.until(rules.keys.size).foreach(i => index2TicketFieldName += (i -> rules.keySet))

    //mark fields that ticket position *cannot* possibly be and start trimming
    valids
      .map(v => ticket2ImpossibleFields(v, rules))
      .foreach(l => {
        val index = l.head._2
        val field = l.head._1.head

        index2TicketFieldName.update(index, index2TicketFieldName(index) - field)
      })

    val answerIndices: collection.mutable.ListBuffer[Int] = ListBuffer()

    //to make an answer you'll need at least a field that has only one possible match and take it from there.
    //take the only sure field you have and deduce from there until there's no more singles

    var single = getSingle(index2TicketFieldName)

    while (singlesExist(index2TicketFieldName)) {
      single = getSingle(index2TicketFieldName)

      if (single._2.head contains "departure") {
        answerIndices.addOne(single._1)
      }

      removeFoundSingle(index2TicketFieldName, single._2)
    }

    println(myTicket
      .zipWithIndex
      .filter(p => answerIndices contains p._2)
      .map(e => e._1.toLong) //overflows otherwise
      .product)
  }

  private def removeFoundSingle(index2TicketFieldName: mutable.Map[Int, Set[String]], single: Set[String]): Unit = {
    index2TicketFieldName
      .foreach(p =>
        index2TicketFieldName.update(p._1, index2TicketFieldName(p._1).removedAll(single)))
  }

  private def getSingle(index2TicketFieldName: mutable.Map[Int, Set[String]]) = {
    index2TicketFieldName.filter(p => p._2.size == 1).head
  }

  private def singlesExist(index2TicketFieldName: mutable.Map[Int, Set[String]]) = {
    index2TicketFieldName.exists(l => l._2.size == 1)
  }

  def ticket2ImpossibleFields(t: List[Int], rules: Map[String, List[Range]]): List[(List[String], Int)] = {
    t.map(field => {
      rules.filterNot(e => {
        val range1 = e._2.head
        val range2 = e._2.last
        (range1 contains field) || (range2 contains field)
      })
        .keySet
        .toList
    })
      .zipWithIndex
      .filterNot(p => p._1.isEmpty)
  }
}
