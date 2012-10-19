package problem_014

import common._

object Answer3 extends Answer {

    override def title = "Using yield return."

    def answer = {
        var start = 999999L
        var (m, result) = (0L, start)
        while (start > 500000) {
            val tmp = iterateCollatz(start).size
            if (tmp > m) {
                m = tmp
                result = start
            }
            start -= 2
        }
        result
    }

    def iterateCollatz(start: Long) = generator {
        if (start >= 1) {
            yield_return(start)
            var n = start
            while (n > 1) {
                n = if (isEven(n)) n >> 1 else (n << 2) - n + 1
                yield_return(n)
            }
        }
    }

}
