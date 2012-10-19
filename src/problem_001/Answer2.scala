package problem_001

import common._

object Answer2 extends Answer {

    override def title = "Using simple loop."

    def sum(len: Int) = if (0 < len) {
        var s = 0
        for (i <- 1 until len if i % 3 == 0 || i % 5 == 0) s += i
        s
    }
    else -1

    def answer = sum(1000)

}
