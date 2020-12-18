package themis.aoc2020
package day17

import themis.aoc2020.day11.day11.{determineNewState, getFromGrid, getNeighbors, getNeighbors2, gridsDiffer, run}

import scala.io.Source
import scala.util.Using

object day17 {

  type Grid3 = collection.mutable.Map[Point3, Char]
  type Grid4 = collection.mutable.Map[Point4, Char]

  type Point3 = (Int, Int, Int)
  type Point4 = (Int, Int, Int, Int)

  def determineNewState(c: Char, neighbors: List[Point3], grid3: Grid3): Char = {
    val neighVals = neighbors.map(tup => grid3(tup))
    if (c == '.') {
      if (neighVals.count(ch => ch.toString == "#") == 3) '#' else '.'
    } else if (c == '#') {
      if (Set(2, 3) contains neighVals.count(ch => ch == '#')) '#' else '.'
    } else {
      c
    }
  }

  def determineNewState4d(c: Char, neighbors: List[Point4], grid4: Grid4): Char = {
    val neighVals = neighbors.map(tup => grid4(tup))
    if (c == '.') {
      if (neighVals.count(ch => ch.toString == "#") == 3) '#' else '.'
    } else if (c == '#') {
      if (Set(2, 3) contains neighVals.count(ch => ch == '#')) '#' else '.'
    } else {
      c
    }
  }

  def run3d(grid: Grid3, x: Int, y: Int, z: Int): Grid3 = {
    var g = grid
    val newGrid: Grid3 = collection.mutable.Map(grid.toSeq: _*).withDefaultValue('.') // le copy trix


    g = collection.mutable.Map(newGrid.toSeq: _*).withDefaultValue('.')
    for (x <- -(x + 1) to x + 1) {
      for (y <- -(y + 1) to y + 1) {
        for (z <- -(z + 1) to z + 1) {
          val neighbors = getNeighbors3d(x, y, z)
          val c = determineNewState(g(x, y, z), neighbors, g)
          newGrid += (x, y, z) -> c
        }
      }
    }
    newGrid
  }

  def run4d(grid: Grid4, x: Int, y: Int, z: Int, w: Int): Grid4 = {
    var g = grid
    val newGrid: Grid4 = collection.mutable.Map(grid.toSeq: _*).withDefaultValue('.') // le copy trix

    g = collection.mutable.Map(newGrid.toSeq: _*).withDefaultValue('.')
    for (x <- -(x + 1) to x + 1) {
      for (y <- -(y + 1) to y + 1) {
        for (z <- -(z + 1) to z + 1) {
          for (w <- -(w + 1) to w + 1) {

            val neighbors = getNeighbors4d((x, y, z, w))
            val c = determineNewState4d(g(x, y, z, w), neighbors, g)

            newGrid += (x, y, z, w) -> c
          }
        }
      }
    }
    newGrid
  }

  def getNeighbors4d(p: Point4): List[Point4] = {
    val dx = List(0, 1, -1)
    val dy = List(0, 1, -1)
    val dz = List(0, 1, -1)
    val dw = List(0, 1, -1)
    val x = p._1
    val y = p._2
    val z = p._3
    val w = p._4

    dx.flatMap(
      xx =>
        dy.flatMap(
          yy =>
            dz.flatMap(zz =>
              dw.map(ww =>
                (xx + x, yy + y, zz + z, ww + w)
              )))).tail //disregard self, where diff is 0 for every dim
  }

  def getNeighbors3d(x: Int, y: Int, z: Int): List[Point3] = {
    val dx = List(0, 1, -1)
    val dy = List(0, 1, -1)
    val dz = List(0, 1, -1)

    dx.flatMap(
      xx =>
        dy.flatMap(
          yy =>
            dz.map(zz =>
              (xx + x, yy + y, zz + z)
            ))).tail //disregard (dx,dy,dz)=(0,0,0)
  }

  def main(args: Array[String]): Unit = {
    //OH HELLO CONWAY'S GAME OF LIFE... AGAIN!

    val og = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day17.txt")) {
      source =>
        source.getLines()
          .map(n => n.toList)
          .toList
    }
      .get

    var originalGrid3: Grid3 = collection.mutable.Map().withDefaultValue('.')
    var originalGrid4: Grid4 = collection.mutable.Map().withDefaultValue('.')

    makeGrid3d(og, originalGrid3)

    //part1
    var grid3d = originalGrid3
    for (_ <- 0 until 6) {

      val x = grid3d.maxBy(t => t._1._1)._1._1
      val y = grid3d.maxBy(t => t._1._2)._1._2
      val z = grid3d.maxBy(t => t._1._3)._1._3

      grid3d = run3d(grid3d, x, y, z)
    }
    println(grid3d.values.count(v => v == '#'))

    //part2
    var grid4d = originalGrid4
    makeGrid4d(og, originalGrid4)
    for (_ <- 0 until 6) {

      val x = grid4d.maxBy(t => t._1._1)._1._1
      val y = grid4d.maxBy(t => t._1._2)._1._2
      val z = grid4d.maxBy(t => t._1._3)._1._3
      val w = grid4d.maxBy(t => t._1._4)._1._4

      grid4d = run4d(grid4d, x, y, z, w)

    }
    println(grid4d.values.count(v => v == '#'))

  }

  private def makeGrid4d(og: List[List[Char]], originalGrid4: Grid4): Unit = {
    og.zipWithIndex.foreach(l => {
      val x = l._2
      val ls = l._1.zipWithIndex
      ls.foreach(item => {
        val y = item._2
        val z = 0
        val w = 0
        val v = item._1
        originalGrid4 += (x, y, z, w) -> v
      })
    })
  }

  private def makeGrid3d(og: List[List[Char]], originalGrid3: Grid3): Unit = {
    og.zipWithIndex.foreach(l => {
      val x = l._2
      val ls = l._1.zipWithIndex
      ls.foreach(item => {
        val y = item._2
        val z = 0
        val v = item._1
        originalGrid3 += (x, y, z) -> v
      })
    })
  }
}
