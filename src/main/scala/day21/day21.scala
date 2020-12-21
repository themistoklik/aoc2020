package themis.aoc2020
package day21

import scala.io.Source
import scala.util.Using

object day21 {
  def main(args: Array[String]): Unit = {
    val allergens2Ingredients = collection.mutable.Map[String, Set[String]]()
    val ingr = collection.mutable.Map[String, Int]().withDefaultValue(0)

    val inp: Unit = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day21.txt")) {
      source =>
        source.mkString
          .split("\n")
          .foreach(l => {
            val split = l.split(" \\(contains")
            val ingredients = split.head
            val allergens = split.last
            val v = ingredients.split(" ").map(c => c.trim)

            v.foreach(vv => {
              ingr(vv) += 1
            })

            allergens
              .replace(",", "")
              .replace(")", "")
              .split(" ")
              .filterNot(c => c.isEmpty)
              .toList
              .foreach(a => {
                val k = a.trim

                if (allergens2Ingredients.contains(k)) {
                  allergens2Ingredients.update(k, allergens2Ingredients(k).intersect(v.toSet))
                } else {
                  allergens2Ingredients += k -> v.toSet
                }

              })
          })
    }
      .get

    //part1
    println(ingr.filter(p => {
      val ingredient = p._1
      allergens2Ingredients.values.forall(s => !(s contains ingredient))
    })
      .values
      .sum
    )

    //part2
    var res: collection.mutable.Map[String, String] = collection.mutable.Map()
    while (allergens2Ingredients.exists(p => p._2.size == 1)) {
      val singles = allergens2Ingredients
        .filter(p => p._2.size == 1)

      singles
        .foreach(s => {
          val element = s._2.head
          allergens2Ingredients.remove(s._1)
          res += s._1 -> element

          allergens2Ingredients.foreach(p => {
            if (p._2 contains element) {
              allergens2Ingredients.update(p._1, p._2 - element)
            }
          })
        })
    }

    println(allergens2Ingredients, "<-this should be emptyyy")
    println(res.toList.sortBy(e => e._1).map(e => e._2).mkString(","))
  }
}
