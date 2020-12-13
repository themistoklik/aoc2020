package themis.aoc2020
package day12

import scala.io.Source
import scala.util.Using

object day12 {

  val directions = List("N", "E", "S", "W")

  def move(nextDir: String, howMuch: Int, currPos: (Int, Int, String)): (Int, Int, String) = {
    val currDir = currPos._3
    val currX = currPos._1
    val currY = currPos._2

    val nd = nextDir match {
      case "F" => currDir
      case "L" =>
        val turn = directions((((directions.indexOf(currDir) - (howMuch / 90)) % 4) + 4) % 4)
        return (currX, currY, turn)
      case "R" =>
        val turn = directions((directions.indexOf(currDir) + (howMuch / 90)) % 4)
        return (currX, currY, turn)
      case _ => nextDir
    }

    nd match {
      case "N" => (currX, currY + howMuch, if (nd.isEmpty) nextDir else currDir)
      case "S" => (currX, currY - howMuch, if (nd.isEmpty) nextDir else currDir)
      case "W" => (currX - howMuch, currY, if (nd.isEmpty) nextDir else currDir)
      case "E" => (currX + howMuch, currY, if (nd.isEmpty) nextDir else currDir)
    }

  }

  def move2(nextDir: String, howMuch: Int, currPos: (Int, Int), currWPPOs: (Int, Int)): ((Int, Int), (Int, Int)) = {
    val currX = currPos._1
    val currY = currPos._2

    val currWPX = currWPPOs._1
    val currWPY = currWPPOs._2

    nextDir match {
      case "F" =>
        val newShipPos = (currX + howMuch * currWPX, currY + howMuch * currWPY)
        (newShipPos, (currWPX, currWPY)) //ship moves
      case "L" =>
        (currPos, rotate("L", howMuch, currWPX, currWPY))
      case "R" =>
        (currPos, rotate("R", howMuch, currWPX, currWPY))
      case "N" => (currPos, (currWPX, currWPY + howMuch))
      case "S" => (currPos, (currWPX, currWPY - howMuch))
      case "W" => (currPos, (currWPX - howMuch, currWPY))
      case "E" => (currPos, (currWPX + howMuch, currWPY))
    }

  }

  //honest to god i tried with the cos/sin formula but it's haunted.
  val rotations = Map(
    "R90" -> ((0, 1), (-1, 0)), "R180" -> ((-1, 0), (0, -1)), "R270" -> ((0, -1), (1, 0)),
    "L90" -> ((0, -1), (1, 0)), "L180" -> ((-1, 0), (0, -1)), "L270" -> ((0, 1), (-1, 0)))

  def rotate(d: String, deg: Int, x: Int, y: Int): (Int, Int) = {
    val r = rotations(d + deg.toString)
    (x * r._1._1 + y * r._1._2, x * r._2._1 + y * r._2._2)
  }

  def main(args: Array[String]): Unit = {

    val instructions = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day12.txt")) {
      source =>
        source.getLines()
          .toList
    }
      .get

    var currPos = (0, 0, "E") //x,y,direction

    instructions.foreach(i => {
      val nextDirection = i.head.toString
      val byHowMuch = i.tail.toInt
      currPos = move(nextDirection, byHowMuch, currPos)
    })

    //part1
    println(currPos._1.abs + currPos._2.abs)

    //part2
    var currPos2 = (0, 0) //x,y
    var currWPPos = (10, 1) //10 east, 1 north

    instructions.foreach(i => {
      val nextDirection = i.head.toString
      val byHowMuch = i.tail.toInt
      val r = move2(nextDirection, byHowMuch, currPos2, currWPPos)
      currPos2 = r._1
      currWPPos = r._2
      println(i, currPos2, currWPPos)
    })
    //    println(currPos2)
    println(currPos2._1.abs + currPos2._2.abs)

  }

}
