package themis.aoc2020
package day11

import scala.io.Source
import scala.util.Using

object day11 {

  def GARBAGE: (Int, Int, Char) = (0, 0, '%')

  def getFromGrid(x: Int, y: Int, grid: Array[Array[Char]]): Option[(Int, Int, Char)] = {
    val rows = grid.length
    val cols = grid.head.length
    if ((0 until rows contains x) && (0 until cols contains y)) {
      Option((x, y, grid(x)(y)))
    } else {
      Option.empty
    }
  }

  def getNeighbors2(x: Int, y: Int, grid: Array[Array[Char]]): Array[(Int, Int, Char)] = {
    val dx = List(0, 1, -1)
    val dy = List(0, 1, -1)
    val result = collection.mutable.ListBuffer[(Int, Int, Char)]()
    //00 01 0-1 10 11 1-1 -10 -11 -1-1
    for (xx <- dx) {
      for (yy <- dy) {
        var xdir = xx
        var ydir = yy

        //skip self
        if (!(xx == 0 && yy == 0)) {
          var neigh = getFromGrid(xx + x, yy + y, grid).getOrElse(GARBAGE)._3
          //keep going till you fall off
          while (neigh == '.') {
            xdir += xx
            ydir += yy
            neigh = getFromGrid(xdir + x, ydir + y, grid).getOrElse(GARBAGE)._3
          }
          result.addOne(x, y, if (neigh == '%') '.' else neigh)
        }
      }
    }
    result.toArray
  }

  def getNeighbors(x: Int, y: Int, grid: Array[Array[Char]]): Array[(Int, Int, Char)] = {
    val dx = List(0, 1, -1)
    val dy = List(0, 1, -1)

    dx.flatMap(
      xx =>
        dy.flatMap(
          yy =>
            getFromGrid(xx + x, yy + y, grid)
        ))
      .tail
      .toArray // drop (0,0) pair, self
  }

  def determineNewState(c: Char, neighbors: Array[(Int, Int, Char)], occupiedThreshold: Int): Char = {
    val neighVals = neighbors.map(tup => tup._3)
    if (c == 'L') {
      if (neighVals.count(ch => ch.toString == "#") == 0) '#' else 'L'
    } else if (c == '#') {
      if (neighVals.count(ch => ch.toString == "#") >= occupiedThreshold) 'L' else '#'
    } else {
      c
    }
  }

  def run(grid: Array[Array[Char]], occupiedThreshold: Int, neighs: (Int, Int, Array[Array[Char]]) => Array[(Int, Int, Char)]): Array[Array[Char]] = {
    var g = grid
    val newGrid = grid.transpose.transpose // le copy trix
    do {
      g = newGrid.transpose.transpose
      for (x <- g.indices) {
        for (y <- g.head.indices) {
          val neighbors = neighs(x, y, g)
          val c = determineNewState(g(x)(y), neighbors, occupiedThreshold)
          newGrid(x)(y) = c
        }
      }
    } while (gridsDiffer(g, newGrid))
    newGrid
  }


  private def gridsDiffer(grid: Array[Array[Char]], newGrid: Array[Array[Char]]) = {
    grid.toList.flatten.toString != newGrid.toList.flatten.toString
  }

  private def printGrid(grid: Array[Array[Char]]) = {
    grid foreach { row => row foreach print; println }
  }

  def main(args: Array[String]): Unit = {

    //OH HELLO CONWAY'S GAME OF LIFE
    val ogrid = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day11.txt")) {
      source =>
        source.getLines()
          .map(n => n.toArray)
          .toArray
    }
      .get

    //part1
    var grid = run(ogrid, 4, getNeighbors)
    println(grid.flatten.count(seat => seat == '#'))
    //part2
    grid = run(ogrid, 5, getNeighbors2)
    println(grid.flatten.count(seat => seat == '#'))
  }
}
