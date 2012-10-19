package problem_002

import common._

object Answer3 extends Answer {

    override def title = "Using yield return."

    def fib(cond: (Int) => Boolean) = generator {
        var (i, j) = (1, 2)
        while (cond(j)) {
            yield_return(j)
            val k = i + j
            i = j
            j = k
        }
    }

    def sum() = {
        var s = 0
        for (i <- fib(_ < 4000000) if isEven(i))
            s += i
        s
    }

    def answer = sum()

}
