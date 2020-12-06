package day1

import java.io.File

class Solution1 {
    companion object {

        fun twoSum(target: Int, numbers: Set<Int>): Int {
            val result: MutableSet<Int> = mutableSetOf()

            for (n in numbers) {
                if (target - n in numbers) {
                    result.add(target - n)
                    result.add(n)
                    break
                }
            }


            return when {
                result.isEmpty() -> -1
                else -> result.reduce { a, b -> a * b }

            }
        }

        fun getNumbers() = File("/Users/themis/IdeaProjects/aoc2020/src/main/resources/day1.txt")
            .readLines()
            .map { Integer.valueOf(it) }
            .toSet()

        fun threeSum(numbers: Set<Int>): Int {
            for (n in numbers) {
                val twoSum = twoSum(2020 - n, numbers)
                if (twoSum != -1) return n* twoSum
            }
            return -1
        }

    }
}