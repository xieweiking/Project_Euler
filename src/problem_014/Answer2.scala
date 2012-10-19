package problem_014

import common._

object Answer2 extends Answer {

    override def title = "Using List."

    def answer = {
        var start = 999999L
        var (m, result) = (0L, start)
        while (start > 500000) {
            val tmp = collatzList(start).size
            if (tmp > m) {
                m = tmp
                result = start
            }
            start -= 2
        }
        result
    }

    def collatzList(x: Long): List[Long] = if (x > 1)
        x :: collatzList(if (isEven(x)) x >> 1 else (x << 2) - x + 1)
    else List(1)

}
