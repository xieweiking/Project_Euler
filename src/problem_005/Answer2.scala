package problem_005

import common._

object Answer2 extends Answer {

    override def title = "Using array."

    def answer = {
        val ary = (1L to 20L).toArray
        for (i <- 0 until 20; j <- 0 until i if ary(i) % ary(j) == 0) ary(i) /= ary(j)
        ary.reduce(_ * _)
    }

}
