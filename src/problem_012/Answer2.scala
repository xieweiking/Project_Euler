package problem_012

import common._

object Answer2 extends Answer {

    override def title = "Get divisors count directly."

    def answer = {
        var i, sum, count = 1L
        while (count < 500) {
            i += 1
            sum += i
            count = countDivisors(sum)
        }
        sum
    }

    def countDivisors(max: Long) = {
        var result = 0L
        for (i <- 1L until math.sqrt(max).round) if (max % i == 0) result += 2
        result
    }

}
