package themis.aoc2020
package day19

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

object day19 {
  type Rule = List[List[String]]
  type Matcher = (String) => (Boolean, String)


  def letterMatcher(letter: String): Matcher = {

    def matcher(inp: String): (Boolean, String) = {
      if (inp.startsWith(letter)) {
        return (true, inp.tail)
      } else {
        return (false, inp)
      }
    }

    matcher
  }

  //exit immediately on true
  def orMatcher(matchers: List[Matcher]): Matcher = {
    def matcher(inp: String): (Boolean, String) = {
      val original = inp
      var s = inp
      for (m <- matchers) {
        val res = m(s)
        s = res._2
        if (res._1) {
          return (res._1, s)
        }
      }
      (false, original)
    }

    matcher
  }

  //exit immediately on false
  def compMatcher(matchers: List[Matcher]): Matcher = {
    def matcher(inp: String): (Boolean, String) = {
      val original = inp
      var s = inp
      var res = (false, original)
      for (m <- matchers) {
        res = m(s)
        s = res._2
        if (!res._1) {
          return (false, original)
        }
      }
      (res._1, s)
    }

    matcher
  }

  def buildMatcher(rule: String, rules: Map[Int, String]): Matcher = {
    if (rule.startsWith("\"") && rule.endsWith("\"")) {
      letterMatcher(rule.replaceAll("\"", "").trim)
    } else if (rule.contains("|")) {
      orMatcher(rule.split("\\|").toList.map(r => buildMatcher(r.trim, rules)))
    } else {
      compMatcher(rule.split(" ").toList.map(r => buildMatcher(rules(r.toInt), rules)))
    }
  }

  def main(args: Array[String]): Unit = {
    val inp = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day19.txt")) {
      source =>
        source.mkString
    }
      .get
      .split("\\n\\n")
      .toList

    var rules = inp.head
    val candidates = inp.last.split("\n").toList

    val r = rules
      .split("\n")
      .map(s => {
        val split = s.split(": ")
        split.head.trim.toInt -> split.last.trim
      })
      .toMap

    // part1
    //    val r0matcher = buildMatcher(r(0), r)
    //    println(candidates.map(c => r0matcher(c))
    //      .filter(pr => pr._1 && pr._2.isEmpty).count(p => p._1)
    //    )


    val r42matcher = buildMatcher(r(42), r)
    val r31matcher = buildMatcher(r(31), r)
    var r42count = 0
    var r31count = 0
    var go = true
    val matches: collection.mutable.ListBuffer[Boolean] = ListBuffer()
    for (c <- candidates) {
      var m = c
      r42count = 0
      r31count = 0
      go = true
      while (go) {
        val r42res = r42matcher(m)
        go = r42res._1
        if (go) {
          r42count += 1
        }
        m = r42res._2
      }
      go = true
      while (go) {
        val r31res = r31matcher(m)
        go = r31res._1
        if (go) {
          r31count += 1
        }
        m = r31res._2
      }
      val b = m.isEmpty && r42count > r31count && r31count > 0
      if(b){
        matches.append(b)
      }
    }
    println(matches.size)
  }

}
