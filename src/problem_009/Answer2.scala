package problem_009

import common._

object Answer2 extends Answer {

    override def title = "Using middle variable composite."

    def answer = {
        val (a, b, c) = findTriplet(1000)
        a * b * c
    }

    def findTriplet(sum: Int): (Int, Int, Int) = {
        var m, n = 0
        while (n < sum) {
            val n2 = n * n
            val sqrtDelta = math.sqrt(n2 + 2 * sum)
            val minusN = -n
            val (m0, m1) = ((minusN + sqrtDelta) / 2, (minusN - sqrtDelta) / 2)
            if (isInteger(m0) && m0 > n) m = m0.asInstanceOf[Int]
            else if (isInteger(m1) && m1 > n) m = m1.asInstanceOf[Int]
            if (m != 0) {
                val m2 = m * m
                return (2 * m * n, m2 - n2, m2 + n2)
            }
            n += 1
        }
        (0, 0, 0)
    }

}
