package problem_014

import common._
import scala.collection.mutable

object Answer4 extends Answer {

    override def title = "Using cache."

    def answer = {
        val cache = mutable.Map[Long, Long](1L -> 1L)
        var start = 999999L
        var (m, result) = (0L, start)
        while (start > 500000) {
            val tmp = if (cache contains start) cache(start) else {
                var (count, next, cachedNext) = (0L, start, false)
                do {
                    next = calcNext(next)
                    cachedNext = cache contains next
                    count += (if (cachedNext) cache(next) else 1)
                } while (next > 1 && !cachedNext)
                cache(start) = count
                count
            }
            if (tmp > m) {
                m = tmp
                result = start
            }
            start -= 2
        }
        result
    }

    def calcNext(n: Long) = if (isEven(n)) n >> 1 else (n << 2) - n + 1

}
