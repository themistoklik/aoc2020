package themis.aoc2020
package day14

import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.mutable.ListBuffer
import scala.collection.{BitSet, BitSetOps, mutable}
import scala.{:+, ::}
import scala.io.Source
import scala.util.Using

object day14 {

  //in goes mask out goes what value to set at what index
  def parseMask(mask: String, exclude: Char): List[(Char, Int)] =
    mask.split("mask=")
      .last
      .reverse
      .zipWithIndex
      .filterNot(p => p._1 == exclude)
      .toList

  def pad0to36(s: String): String = "0" * (36 - s.length) + s

  def toBitSet(n: Long): ListBuffer[Int] = {
    val indexOfOnes = pad0to36(n.toBinaryString)
      .reverse
      .zipWithIndex
      .filter(p => p._1 == '1')
      .map(p => p._2)
      .toList

    List.tabulate(36) { i => if (indexOfOnes contains i) 1 else 0 }.to(ListBuffer)
  }

  def toBitSet2(n: Long): ListBuffer[Char] = {
    val indexOfOnes = pad0to36(n.toBinaryString)
      .reverse
      .zipWithIndex
      .filter(p => p._1 == '1')
      .map(p => p._2)
      .toList

    List.tabulate(36) { i => if (indexOfOnes contains i) '1' else '0' }.to(ListBuffer)
  }

  def genAddresses(m: ListBuffer[Char]): ListBuffer[ListBuffer[Char]] = {
    val at = m.indexOf('X')
    if (at != -1) {
      ListBuffer(m.updated(at, '1'), m.updated(at, '0'))
    } else {
      ListBuffer.empty
    }
  }

  def applyMaskToVal2(mask: List[(Char, Int)], num: Long): mutable.ListBuffer[Long] = {
    val bs = toBitSet2(num)
    mask.foreach(pair => {
      val value = pair._1
      val index = pair._2

      if (value == 'X') bs(index) = 'X' else bs(index) = '1'
    })
    var as: ListBuffer[ListBuffer[Char]] = ListBuffer(bs)

    while (as.forall(a => a.indexOf('X') != -1)) {
      as.foreach(_ => {
        val fz = as.head
        as = as.tail
        genAddresses(fz).foreach(a => as += a)
      })
    }

    as.map(l => java.lang.Long.parseLong(l.reverse.mkString, 2))
  }

  def applyMaskToVal(mask: List[(Char, Int)], num: Long): Long = {
    val bs = toBitSet(num)
    //(val,index)
    mask.foreach(pair => {
      val value = pair._1
      val index = pair._2

      if (value == '0') bs(index) = 0 else bs(index) = 1
    })
    java.lang.Long.parseLong(bs.reverse.mkString, 2)
  }

  def main(args: Array[String]): Unit = {

    val state = collection.mutable.Map[Long, Long]().withDefaultValue(0L)

    val programs = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day14.txt")) {
      source =>
        source.getLines()
          .map(i => {
            i.replaceAll("\\s", "")
              .replace("[", "")
              .replace("]", "")
              .replace("mem", "")
          })
          .toList
    }
      .get
      /**
       * List(
       * List(mask=XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX01, 8=11, 7=101, 8=0)
       * List(mask=XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X, 8=11, 7=101, 8=0)
       * )
       */
      .foldLeft(List.empty[List[String]])({
        case (acc, i) if acc.isEmpty => List(List(i))
        case (acc, i) if i.contains("mask") => acc :+ List(i)
        case (acc, i) => acc.init :+ (acc.last :+ i) //stick it to the last elem of List[List[String]]
      })

    programs
      .foreach(group => {
        val mask = parseMask(group.head, 'X')
        val rs = group.tail
        rs.foreach(r => {
          val split = r.split("=")
          val memoryAddress = split.head.toLong
          val value = split.last.toLong
          val finalv = applyMaskToVal(mask, value)

          state.put(memoryAddress, finalv)
        })
      })

    //part1
    println(state.values.sum)

    //part2
    val state2 = collection.mutable.Map[Long, Long]().withDefaultValue(0L)

    programs.foreach(group => {
      val mask = parseMask(group.head, '0')
      val rs = group.tail
      rs.foreach(r => {
        val split = r.split("=")
        val memoryAddress = split.head.toLong
        val value = split.last.toLong
        val addresses = applyMaskToVal2(mask, memoryAddress)

        addresses.foreach(a =>
          state2.put(a, value))
      })
    })

    println(state2.values.sum)
  }
}
