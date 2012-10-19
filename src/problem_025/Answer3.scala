package problem_025

import common._

object Answer3 extends Answer {

    override def title = "Using yield return."

    def fib(cond: (BigInt) => Boolean) = generator {
        var (i, j) = (BigInt(1), BigInt(2))
        while (cond(j)) {
            yield_return(j)
            val k = i + j
            i = j
            j = k
        }
    }

    def answer = {
        var i = 0
        for (f <- fib(_.toString.length < 1000)) i += 1
        i + 3
    }

}
