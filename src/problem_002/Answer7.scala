package problem_002

import common._

object Answer7 extends Answer {

    override def title = "Using lazy stream."

    lazy val fib: Stream[Int] = 1 #:: 1 #:: (fib zip fib.tail).map(t => t._1 + t._2)

    def sum() = {
        var s, i, f = 0
        do { // stream could be used in "for" loop as a range too, but "while" is faster
            f = fib(i)
            if ((f % 2) == 0) s += f
            i += 1
        } while (f < 4000000)
        s
    }

    def answer = sum()

}
