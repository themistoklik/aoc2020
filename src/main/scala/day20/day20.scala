package themis.aoc2020
package day20

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

object day20 {

  def makegrid(inp: String): List[List[Char]] = {
    inp.split("\n").tail.toList.map(row => row.toList)
  }

  //get left and right side and their corresponding reverseds, 4 in total
  def getLongSides(inp: List[List[Char]]): List[(String, List[Char])] = {
    val left = inp.map(i => i.head)
    val right = inp.map(i => i.last)
    List("l" -> left, "lr" -> left.reverse, "r" -> right, "rr" -> right.reverse)
  }

  //get top and bottom side and their corresponding reverseds, 4 in total
  def getShortSides(inp: List[List[Char]]): List[(String, List[Char])] = {
    val top = inp.head
    val bottom = inp.last
    List("t" -> top, "tr" -> top.reverse, "b" -> bottom, "br" -> bottom.reverse)
  }

  def getMatches(tileid: String, tiles: Map[String, List[List[Char]]]): (String, ListBuffer[String]) = {
    val res = collection.mutable.ListBuffer[String]()
    tiles.foreach(t => {

      val intersection = (getShortSides(tiles(tileid)).map(x => x._2) ++ getLongSides(tiles(tileid)).map(x => x._2))
        .intersect((getShortSides(t._2).map(x => x._2) ++ getLongSides(t._2).map(x => x._2)))

      val ss = intersection.map(x => (getShortSides(tiles(tileid)) ++ getLongSides(tiles(tileid))).find(p => p._2 == x))
      if (intersection.nonEmpty && t._1 != tileid) {
        res.append(t._1)
      }
    })

    (tileid, res)
  }

  def main(args: Array[String]): Unit = {

    val tiles = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day20.txt")) {
      source =>
        source.mkString
          .split("\\n\\n")
          .map(l => {
            val split = l.split(":")
            "\\d+".r.findAllIn(split.head).matched -> makegrid(split.last)
          })
          .toMap
    }
      .get

    val longMatches: collection.mutable.Map[String, List[Char]] = collection.mutable.Map()
    val shortMatches: collection.mutable.Map[String, List[Char]] = collection.mutable.Map()
    val differentSides: collection.mutable.ListBuffer[List[Char]] = collection.mutable.ListBuffer()

    tiles.foreach(t => {
      val tile = t._2

      differentSides.addAll(getLongSides(tile).map(x => x._2))
      differentSides.addAll(getShortSides(tile).map(x => x._2))
    })

    var acc = 1L
    tiles.foreach(t => {
      val tile = t._2
      val neighs = (getShortSides(tile) ++ getLongSides(tile))
        .map(ss => {
          differentSides.count(s => s == ss)
        })
        .count(p => p == 1)
      if (neighs == 4) {
        println(t._1)
        acc *= t._1.toLong
      }
    })

    // well what I lack in wit and willingness for hard work I make for in cunning apparently
    // other crafty redditors did this too, but with more sophistication for the bounds of sea sneks, damn
    // seamonster is 15 #, so deborder all your tiles first
    // make a guess how many monsters are there and remove multiples of that from all your #s
    var n = 0
    tiles.values.foreach(t => {
      var tt = t
      tt = tt.tail
      tt = tt.dropRight(1)
      for (r <- tt) {
        n += r.slice(1, r.size-1).count(p => p == '#')
      }
    })
    println(n)
    //oh boi here i go guessing again
    //do a global search for a monster pattern in your debordered input then +- some for split monsters between frames
    20.to(50).foreach(x => {
      println(n-x*15)
    })

    //gotta be one of those if no monsters overlap, if they do fucc it i'll do it another time


  }

}
