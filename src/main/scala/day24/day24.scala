package themis.aoc2020
package day24

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object day24 {
  //https://stackoverflow.com/a/15524441
  //AIN'T YOUR NORMAL X,Y NEIGHBORHOOD STUPID, RECTANGLES BECAME HEXAGONS, THAT CHANGES THINGS OMG
  val neighbors: Map[String, Point] = Map(
    "e" -> (1, 0),
    "w" -> (-1, 0),
    "se" -> (0, -1),
    "sw" -> (-1, -1),
    "ne" -> (1, 1),
    "nw" -> (0, 1),
  )

  type Point = (Int, Int)

  def main(args: Array[String]): Unit = {

    val input: Seq[String] = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day24.txt")) {
      source =>
        source.getLines().toSeq
    }
      .get.toList


    val tiles: collection.mutable.Map[Point, Int] = collection.mutable.Map().withDefaultValue(0)


    var currAt: Point = (0, 0)
    input.foreach(tile => {
      "e|se|sw|nw|ne|w".r.findAllIn(tile).foreach(
        move => {
          val dx = neighbors(move)._1
          val dy = neighbors(move)._2
          currAt = (currAt._1 + dx, currAt._2 + dy)
        })

      tiles.update(currAt, tiles(currAt) + 1)
      currAt = (0, 0)
    })

    //part1
    println(tiles.filter(pair => pair._2 % 2 == 1).values.sum)

    var tilesAfterDay = tiles

    //part2
    1 to 100 foreach {
      _ =>
        tilesAfterDay = tileDay(tilesAfterDay)
    }
    println("%d".formatted(tilesAfterDay.count(p => p._2 % 2 == 1)))

  }

  type Grid = mutable.Map[Point, Int]

  def tileDay(grid: Grid): Grid = {
    val newTiles: Grid = collection.mutable.Map()
    val point2black: Grid = collection.mutable.Map().withDefaultValue(0)

    val black = (x: Int) => {
      x % 2 == 1
    }

    //will also contain out of grid points
    grid
      .filter(t => black(t._2))
      .foreach(tile => {
        val point = tile._1
        neighbors.values.map(v => {
          val dx = v._1
          val dy = v._2
          point2black((point._1 + dx, point._2 + dy)) += 1
        })

      })


    grid
      .filter(t => black(t._2))
      .foreach(black => {
        val neighbors = point2black(black._1)
        if (neighbors == 0 || neighbors > 2) {
          newTiles.update(black._1, 0)
        } else {
          newTiles.update(black._1, 1)
        }
      })

    val tilesWith2BlackNeighbors = point2black
      .filter(x => x._2 == 2)

    tilesWith2BlackNeighbors
      .foreach(x => {
        //even if it's out of grid now it's gotta be, cause it's a white boundary turned black
        if (grid.getOrElse(x._1, 0) % 2 == 0) {
          newTiles.update(x._1, 1)
        }
      })

    newTiles
  }

}
