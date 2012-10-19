package problem_002

import common._
import scala.annotation.tailrec

object Answer5 extends Answer {

    override def title = "Using tail recursive."

    def fib(n: Int): Int = {
        @tailrec
        def f(cur: Int, next: Int, idx: Int): Int =
            if (idx == 0) cur
            else f(next, cur + next, idx - 1)

        f(1, 2, n)
    }

    def sum() = {
        var s, i, f = 0
        do {
            f = fib(i)
            if ((f % 2) == 0) s += f
            i += 1
        } while (f < 4000000)
        s
    }

    def answer = sum()

}
