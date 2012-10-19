package problem_024

import common.Answer
import scala.collection.mutable

object Answer2 extends Answer {
    
    override def title = "Test the Coefficient of Factorial."

    def answer = nthOfLexicographicPermutation(1000000, 10)

    def nthOfLexicographicPermutation(nth: Int, digits: Int) = {
        val digitSeq = 0 to digits - 1
        if (nth <= 1)
            digitSeq.map(_.toString).reduce(_ + _)
        else {
            val factorials = new Array[Int](digits + 1)
            for (i <- digitSeq) factorials(i) =
                if (i == 0) 1
                else i * factorials(i - 1)
            factorials(digits) = digits * factorials(digits - 1)
            val total = factorials(digits)
            val revDigitSeq = digitSeq.reverse
            if (total == nth)
                revDigitSeq.map(_.toString).reduce(_ + _)
            else if (total > nth) {
                val chars = mutable.ArrayBuffer() ++ digitSeq
                val strBuilder = new StringBuilder
                var sum = 0
                for (digit <- revDigitSeq) {
                    val factorial = factorials(digit)
                    var coefficient = digit
                    sum += factorials(digit + 1) - factorial
                    while (sum >= nth) {
                        coefficient -= 1
                        sum -= factorial
                    }
                    strBuilder.append(chars.remove(coefficient))
                }
                strBuilder.toString
            }
            else
                throw new IllegalStateException
        }
    }

}
