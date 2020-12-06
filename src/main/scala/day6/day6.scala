package themis.aoc2020
package day6

import scala.io.Source
import scala.util.Using

object day6 {
  def main(args: Array[String]): Unit = {
    val lines = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day6.txt")) {
      source => source.mkString
    }

    println("part1")
    val groupAnswers = lines
      .get
      .split("\\n\\n")
      .toList

    println(
      groupAnswers
        .map(groupAnswer => groupAnswer.replace("\n", "").toSet)
        .map(answers => answers.size)
        .sum
    )

    println("part2")
    println(
      groupAnswers
        .map(groupAnswer =>
          groupAnswer.split("\n")
            .map(answer => answer.toSet)
            .reduce((a, b) => a intersect b))
        .map(groupAnswers => groupAnswers.size)
        .sum
    )
  }
}

