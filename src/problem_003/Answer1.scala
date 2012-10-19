package problem_003

import common._

object Answer1 extends Answer {

    override def title = "Using odd Prime divide test table."

    def maxPrimeFactorOf(x: Long) = {
        var num = x.abs
        var max = num // num may be prime itself
        var prime = 0L
        val primes = Primes()
        do {
            prime = primes.next
            if (num % prime == 0) {
                num /= prime
                max = prime
            }
        } while (prime <= num)
        max
    }

    def answer = maxPrimeFactorOf(600851475143L)

}
