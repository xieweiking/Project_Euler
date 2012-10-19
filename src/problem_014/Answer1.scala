package problem_014

import common._

object Answer1 extends Answer {

    def answer = {
        var start = 999999L
        var (m, result) = (0L, start)
        while (start > 500000) {
            val tmp = {
                var (count, next) = (0L, start)
                do {
                    next = if ((next & 1) == 0) next >> 1 else (next << 2) - next + 1
                    count += 1
                } while (next > 1)
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

}
