package problem_002

import common._

object Answer6 extends Answer {

    override def title = "Using formula."

    lazy val sqrtFive = math.sqrt(5)
    lazy val x = (1 + sqrtFive) / 2
    lazy val y = (1 - sqrtFive) / 2

    /**
     * fib(n) = (((1 + sqrt(5)) / 2) ^ n - ((1 - sqrt(5)) / 2) ^ n) / sqrt(5)
     */
    def fib(n: Long) = math.round(((math.pow(x, n) - math.pow(y, n)) / sqrtFive))

    def sum() = {
        var s, i, f = 0L
        do {
            f = fib(i)
            if ((f % 2) == 0) s += f
            i += 1
        } while (f < 4000000)
        s
    }

    def answer = sum()

}
