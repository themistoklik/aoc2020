package themis.aoc2020
package day25

object day25 {

  def main(args: Array[String]): Unit = {
    val v1: Long = 8252394
    val v2: Long = 6269621

    var sn = 1L

    0 to 20000000 foreach { x =>
      sn *= 7
      sn = sn % 20201227
      if (sn == v1) {
        println("1",x)
      }
    }

    val ls: Long = 18739603
    sn = 1L
    0 to ls.toInt foreach {x=>
      sn *= v2
      sn = sn % 20201227
    }
    println("end", sn)
  }
}
