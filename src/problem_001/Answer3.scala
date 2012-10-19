package problem_001

import common._

object Answer3 extends Answer {

    override def title = "Using formula."

    def sum(len: Int) = if (0 < len) {
        val n = len - 1
        sumOfArithmeticSeq(3, 3, n) + sumOfArithmeticSeq(5, 5, n) - sumOfArithmeticSeq(15, 15, n)
    }
    else -1

    def answer = sum(1000)

}
