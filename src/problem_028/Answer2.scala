package problem_028

import common._

object Answer2 extends Answer {

    override def title = "Sum the sum of arithmetic sequense of every level with nthEven(leve) delta."

    def answer = {
        var prevMax = 1L // (0, 0)
        var sum = prevMax
        for (level <- 1 to seqCount(1001)) {
            val (max, edge) = Spiral.maxAndEdgeOf(level)
            sum += sumOfArithmeticSeq(prevMax + edge, edge, max)
            prevMax = max
        }
        sum
    }

    def seqCount(max: Int) = {
        require(isOdd(max) && max >= 1)
        (max - 1) / 2
    }

}
