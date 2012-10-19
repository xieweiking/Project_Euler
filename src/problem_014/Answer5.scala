package problem_014

import common._

object Answer5 extends Answer {

    override def title = "Using Stream."

    def answer = {
        var start = 999999L
        var (m, result) = (0L, start)
        while (start > 500000) {
            val tmp = collatzStream(start).size
            if (tmp > m) {
                m = tmp
                result = start
            }
            start -= 2
        }
        result
    }

    def collatzStream(x: Long): Stream[Long] = if (x > 1)
        x #:: collatzStream(if (isEven(x)) x >> 1 else (x << 2) - x + 1)
    else Stream(1)

}
