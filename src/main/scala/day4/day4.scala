package themis.aoc2020
package day4

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object day4 {
  def validateNumberIsLengthAndInRange(field: String, length: Int, startRange: Int, endRange: Int): Boolean =
    field.length == length && (startRange to endRange contains field.toInt)

  def validateEcl(field: String): Boolean = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth") contains field

  def validateHcl(field: String): Boolean = field.startsWith("#") && field.drop(1).forall(p => p.isLetterOrDigit)

  def validateByr(field: String): Boolean = validateNumberIsLengthAndInRange(field, 4, 1920, 2002)

  def validateIyr(field: String): Boolean = validateNumberIsLengthAndInRange(field, 4, 2010, 2020)

  def validateEyr(field: String): Boolean = validateNumberIsLengthAndInRange(field, 4, 2020, 2030)

  def validateHgt(field: String): Boolean = {
    if (field contains "cm") {
      validateNumberIsLengthAndInRange(field.dropRight(2), 3, 150, 193)
    } else if (field contains "in") {
      validateNumberIsLengthAndInRange(field.dropRight(2), 2, 59, 76)
    } else {
      false
    }
  }

  def validatePid(field: String): Boolean = field.length == 9

  def validateCid(field: String): Boolean = true

  val fieldValidators: mutable.Map[String, String => Boolean] = collection.mutable.Map(
    "ecl" -> validateEcl,
    "hcl" -> validateHcl,
    "byr" -> validateByr,
    "eyr" -> validateEyr,
    "iyr" -> validateIyr,
    "hgt" -> validateHgt,
    "pid" -> validatePid,
    "cid" -> validateCid)

  def isValid(passportLine: String): Boolean = {
    val validKeys = collection.mutable.Map(
      "ecl" -> false,
      "hcl" -> false,
      "byr" -> false,
      "eyr" -> false,
      "iyr" -> false,
      "hgt" -> false,
      "pid" -> false,
      "cid" -> false)

    validKeys.keySet.foreach(
      key => if (passportLine.contains(key)) {
        validKeys.update(key, true)
      })

    val allAreThere = validKeys.values.forall(value => value)
    val missingKeys = validKeys.filterInPlace((_, v) => !v)
    val onlyCidMissing = missingKeys.size == 1 && missingKeys.contains("cid")
    if (allAreThere || onlyCidMissing) true else false
  }

  def isValidPart2(passportLine: String): Boolean = {
    val passportLines = passportLine.split(" ")
      .map(field => {
        val split = field.split(":")
        split(0) -> split(1)
      }).toMap

    val validKeys = collection.mutable.Map(
      "ecl" -> false,
      "hcl" -> false,
      "byr" -> false,
      "eyr" -> false,
      "iyr" -> false,
      "hgt" -> false,
      "pid" -> false,
      "cid" -> false)

    validKeys.keySet.foreach(k =>
      if (passportLines contains k) {
        val validationResult = fieldValidators(k).apply(passportLines(k))
        validKeys.update(k, validationResult)
      }
    )

    val allAreThere = validKeys.values.forall(value => value)
    val missingKeys = validKeys.filterInPlace((_, v) => !v)
    val onlyCidMissing = missingKeys.size == 1 && missingKeys.contains("cid")
    if (allAreThere || onlyCidMissing) true else false
  }

  def main(args: Array[String]): Unit = {
    val lines = Using(Source.fromFile("/Users/themis/IdeaProjects/aoc2020.scala/src/main/scala/inputs/day4.txt")) {
      source => source.mkString
    }
    val passportLines = lines
      .get
      .split("\\n\\n")
      .toList
      .map(passport => passport.replace("\n", " "))
      .filterNot(entry => entry.isEmpty)

    println(passportLines.map(isValid).count(v => v))
    println(passportLines.map(isValidPart2).count(v=>v))

  }

}
