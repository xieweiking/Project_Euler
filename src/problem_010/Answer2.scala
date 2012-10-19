package problem_010

import common._

object Answer2 extends Answer {

    override def title = "Odd number table filtering."

    def answer = sumOfPrimesBlow(2000000)

    def sumOfPrimesBlow(max: Int) = {
        var sum = sumOfArithmeticSeq(3, 2, max) + 2
        val table = new Array[Boolean](max / 2 - (if (isEven(max)) 1 else 0))
        for {
            p <- Primes.asSeqBelow(math.sqrt(max).ceil.asInstanceOf[Int]).tail
            i <- 0 until table.size
        } if (!table(i)) {
            val num = i * 2 + 3
            if (num != p && num % p == 0) {
                table(i) = true
                sum -= num
            }
        }
        sum
    }

}
