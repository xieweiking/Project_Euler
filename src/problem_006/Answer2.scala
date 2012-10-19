package problem_006

import common._

object Answer2 extends Answer {

    override def title = "Using formula."

    def answer = diffOfSumSquareAndSquareSum(100)

    def diffOfSumSquareAndSquareSum(n: Long) = {
        val sumSeq = sumOfArithmeticSeq(1, 1, 100)
        sumSeq * sumSeq - n * (n + 1) * (2 * n + 1) / 6
    }

}
