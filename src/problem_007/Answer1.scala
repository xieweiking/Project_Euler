package problem_007

import common._

object Answer1 extends Answer {

    def answer = {
        val size = 10001
        var i = 0
        var result = 0L
        val primes = Primes(size)
        while (i < size) {
            result = primes.next
            i += 1
        }
        result
    }

}
