package problem_002

import common._
import scala.util.continuations.{ reset, shift }

object Answer1 extends Answer {

    override def title = "Using continuations."

    def sum() = {
        val fibSeq = Iterator.continually(FibGen.next)
        var s = 0
        for (
            i <- fibSeq.takeWhile(_ < 4000000) if isEven(i)
        ) s += i
        s
    }

    def answer = sum()

    object FibGen {

        private var continuation: Unit => Int = reset {
            shift(identity[Unit => Int]) // feed compiler's cps param
            var (i, j) = (1, 2)
            while (true) {
                shift { c: (Unit => Int) =>
                    continuation = c
                    i // the real return value
                }
                val k = i + j
                i = j
                j = k
            }
            sys.error("never reach here")
        }

        def next = continuation()

    }

}
