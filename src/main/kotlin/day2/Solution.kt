package day2

import java.io.File

data class PasswordEntry(
    val letter: Char,
    val minOccurs: Int,
    val maxOccurs: Int,
    val passWord: String,
)

class Solution {
    companion object {
        fun String.toPasswordEntry(): PasswordEntry {
            val split = this.split(":")
            val constraint = split.first().split(' ')
            val occurs = constraint.first().split('-')


            return PasswordEntry(
                letter = constraint.last().toCharArray().first(),
                minOccurs = Integer.valueOf(occurs.first()),
                maxOccurs = Integer.valueOf(occurs.last()),
                passWord = split.last().strip()
            )
        }

        private fun PasswordEntry.isValid() =
            if (passWord.filter { it == letter }.count() in minOccurs..maxOccurs) 1 else 0

        private fun PasswordEntry.isValidPart2(): Int {
            val valid = listOf(minOccurs-1, maxOccurs-1)
                .map { passWord.getOrElse(it) { '!' } }
                .filter { it == letter }
                .count() == 1

            return if(valid) 1 else 0
        }


        fun solve1() = File("/Users/themis/IdeaProjects/aoc2020/src/main/resources/day2.txt")
            .readLines()
            .asSequence()
            .map { it.toPasswordEntry() }
            .map { it.isValidPart2() }
            .sum()

    }
}