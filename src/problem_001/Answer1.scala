package problem_001

import common._

object Answer1 extends Answer {

    override def title = "Using Map/Reduce."

    def sum(len: Int) = if (0 < len)
        (for (i <- 1 until len if i % 3 == 0 || i % 5 == 0) yield i).reduce(_ + _)
    else -1

    def answer = sum(1000)

}
