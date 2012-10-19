package problem_048

import common._

object Answer2 extends Answer {

    override def title = "BigInt Brute Force."

    def answer = {
        var sum = BigInt(0)
        for (i <- 1 to 1000)
            sum += BigInt(i).pow(i)
        sum % 10000000000L
    }

}
