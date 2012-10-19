package problem_004

import common._

object Answer2 extends Answer {

    override def title = "Using prime factors test."

    def answer: Int = {
        var prefix = 9999
        while (prefix >= 1000) {
            val palindromicNum = prefix * 10000 + prefix.toString.reverse.toInt
            val factors = primeFactorsOf(palindromicNum)
            val length = factors.length
            val last = factors.last
            if (last < 10000 && length >= 2) {
                val head = factors.head
                var (left, right) = (head, last)
                if (length > 2) {
                    if (last > 999) {
                        var tmp = head * last
                        if (tmp < 10000) {
                            left = tmp
                            right = factors.slice(1, length - 1).reduce(_ * _)
                        }
                        else {
                            tmp = factors.slice(0, length - 1).reduce(_ * _)
                            if (tmp < 10000) {
                                left = tmp
                                right = last
                            }
                        }
                    }
                    if (left == 0 || right == 0) {
                        val m = length / 2 - 1
                        left = last * factors(m)
                        right = 1
                        var i = 0
                        while (i < length - 1) {
                            if (i != m) right *= factors(i)
                            i += 1
                        }
                    }
                }
                if (left > 999 && left < 10000 && right > 999 && right < 10000) return palindromicNum
            }
            prefix -= 1
        }
        0
    }

}
