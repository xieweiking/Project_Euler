package common

import scala.collection.mutable.ArrayBuffer

final class Primes(initCapacity: Int, primeTest: Long => Boolean = isPrime) {

    private[this] val values = new ArrayBuffer[Long](initCapacity)

    def next = synchronized {
        if (values.isEmpty) {
            values += 2
            2
        }
        else {
            val prime = values.last
            var odd = if (values.length == 1) 3 else prime + 2
            var num = prime
            do {
                if (odd != 5 && odd % 5 == 0) odd += 2
                if (primeTest(odd)) {
                    values += odd
                    num = odd
                }
                else odd += 2
            } while (num == prime)
            num
        }
    }

    def toSeq = synchronized { values.toSeq }

}

final object Primes {

    @inline
    def apply(init: Int = 100, primeTest: Long => Boolean = isPrime) = new Primes(init, primeTest)

    @inline
    def below(x: Long) = Primes((x / ln(x) + x * (
        if (x <= 100L) 0.04
        else if (x <= 1000L) 0.024
        else if (x <= 10000L) 0.0144
        else if (x <= 100000L) 0.00907
        else if (x <= 1000000L) 0.006116
        else if (x <= 10000000L) 0.0044159
        else 0.003)).ceil.asInstanceOf[Int])

    @inline
    def asSeqBelow(x: Long) = {
        val primes = Primes below x
        while (primes.next <= x) {}
        primes toSeq
    }

}
