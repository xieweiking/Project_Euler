package problem_025

import common._

object Answer2 extends Answer {

    override def title = "Using simple loop."

    def answer = {
        var (i, j, count) = (BigInt(1), BigInt(2), 3)
        while (j.toString.length < 1000) {
            val k = i + j
            i = j
            j = k
            count += 1
        }
        count
    }

}
