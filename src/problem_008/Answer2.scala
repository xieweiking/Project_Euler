package problem_008

import common._

object Answer2 extends Answer {

    override def title = "Fork/Join."

    def answer = calcMaxProduct() // the small scale fork/join can't take any advantage..

    def calcMaxProduct(gap: Int = 5, count: Int = CONCURRENT_COUNT, timeout: Long = 50) = {
        val datas = Datas seq
        val size = datas size
        val len = size / count
        val lenGap = len + gap
        val tasks = ConcurrentTasks[Int](timeout)
        for (i <- Range(0, size, len)) tasks fork {
            val seq = datas slice (i, i + lenGap) // slice won't get index out of bound
            var m = 0
            for (j <- 0 until seq.size - gap) m = m max (1 /: seq.slice(j, j + gap))(_ * _)
            m // can not invoke the method just outside this future, invoke the other object's and the future's
            // inner method is ok. this would be a crap idea just like the yield_return's cps problem, damn.
        }
        tasks join (_ reduce (_ max _))
    }

}
