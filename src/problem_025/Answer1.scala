package problem_025

import common._
import scala.annotation.tailrec

object Answer1 extends Answer {

    def answer = {
        var i = 0
        var f = fib(i)
        while (f.toString.length < 1000) {
            i += 1
            f = fib(i)
        }
        i + 2
    }

    def fib(n: Int): BigInt = {
        @tailrec
        def f(cur: BigInt, next: BigInt, idx: Int): BigInt =
            if (idx == 0) cur
            else f(next, cur + next, idx - 1)

        f(1, 2, n)
    }

}
