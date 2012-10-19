package problem_013

import common._

object Answer1 extends Answer {

    def answer = ((BigInt(0) /: Datas.grid)((sum, numSeq) => sum + BigInt(numSeq.mkString))).toString.slice(0, 10)

}
