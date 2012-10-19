package problem_002

import common._

object Answer2 extends Answer {

    override def title = "Using simple loop."

    def sum() = {
        var (s, i, j) = (0, 1, 2)
        do {
            if ((i % 2) == 0) s += i
            val k = i + j
            i = j
            j = k
        } while (i < 4000000)
        s
    }

    def answer = sum()

}
