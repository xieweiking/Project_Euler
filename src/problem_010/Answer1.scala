package problem_010

import common._

object Answer1 extends Answer {

    def answer = {
        var sum = 0L
        val primes = Primes below 2000000
        var p = 0L
        while (p < 2000000) {
            sum += p
            p = primes next
        }
        sum
    }

}
