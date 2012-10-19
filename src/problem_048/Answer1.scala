package problem_048

import common._

object Answer1 extends Answer {

    override def title = "Using Power Mod."

    def answer = {
        var sum = 0L
        val mask = 10000000000L
        for (i <- 1 to 1000)
            sum += powMod(i, i, mask)
        sum % mask
    }

}
