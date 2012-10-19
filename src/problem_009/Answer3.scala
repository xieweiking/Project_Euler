package problem_009

import common._

object Answer3 extends Answer {

    override def title = "Using divide test."

    def answer = {
        val (a, b, c) = findTriplet(1000)
        a * b * c
    }

    def findTriplet(sum: Int): (Int, Int, Int) = {
        val middle = sum / 2
        var a = 1
        while (a < middle) {
            val (m, n) = (sum * (middle - a), sum - a)
            if (m % n == 0) {
                val b = m / n
                return (a, b, sum - a - b)
            }
            a += 1
        }
        (0, 0, 0)
    }

}
