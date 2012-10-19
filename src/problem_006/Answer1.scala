package problem_006

import common._

object Answer1 extends Answer {

    def answer = {
        val sumSeq = sumOfArithmeticSeq(1, 1, 100)
        sumSeq * sumSeq - (1L to 100L).reduce((sum, a) => sum + a * a)
    }

}
