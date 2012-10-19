package problem_012

import common._

object Answer1 extends Answer {

    override def title = "Product of prime factors' powers."

    def answer = {
        var tNum, n, divisors = 1L
        while (divisors < 500) {
            tNum = triangleNumber(n)
            val primeFactors = primeFactorsOf(tNum)
            var (last, count) = (primeFactors.head, 2L)
            divisors = 1
            for (p <- primeFactors.tail) if (p != last) {
                divisors *= count
                last = p
                count = 2
            }
            else count += 1
            divisors = divisors * count - 1
            n += 1
        }
        tNum
    }

    def triangleNumber(n: Long) = sumOfArithmeticSeq(1, 1, n)

}
