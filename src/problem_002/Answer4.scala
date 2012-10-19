package problem_002

import common._

object Answer4 extends Answer {

    override def title = "Using pattern match recursive."

    def fib(n: Int): Int = n match {
        //  case i if i == 0 || i == 1 => 1 // pattern guard is very slow!
        case 0 => 1
        case 1 => 2
        case _ => fib(n - 1) + fib(n - 2)
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
