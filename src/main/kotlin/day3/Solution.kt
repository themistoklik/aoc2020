package day3

import java.io.File

class Solution {
    companion object {
        fun walk(dx: Int = 1, dy: Int = 3): Int {
            val grid = File("/Users/themis/IdeaProjects/aoc2020/src/main/resources/day3.txt")
                .readLines()
                .map { it.toList() }
            grid.forEach {
                println(it)
            }
            var x = 0
            var y = 0
            var trees = 0

            while (true) {
                y = (y + dy) % grid.first().size
                x += dx
                val atTheBottom = x >= grid.size
                if (atTheBottom) break
                if (grid[x][y].toString() == "#") {
                    trees++
                }
            }
            return trees
        }

        fun part2(): Long {

            val nums = listOf(
                1 to 1,
                1 to 3,
                1 to 5,
                1 to 7,
                2 to 1
            )
                .map { (x, y) -> walk(x, y) }
                .map { it.toLong()}

            return nums.reduce { acc, i -> i*acc }

        }


    }
}