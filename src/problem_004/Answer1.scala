package problem_004

import common._

object Answer1 extends Answer {

    override def title = "Using enumerate palindromic test loop."

    def answer = {
        var r = 0
        var i = 9999
        while (i >= 1000) {
            var j = 9999
            while (j >= 1000) {
                val num = i * j
                if (isPalindromic(num) && num > r) r = num
                j -= 1
            }
            i -= 1
        }
        r
    }

    def isPalindromic(n: Int): Boolean = { // using string reverse is slow!
        val s = n.toString
        val m = s.length / 2
        var i = 0
        while (i <= m) {
            if (s(i) != s(s.length - i - 1)) return false
            i += 1
        }
        true
    }

}
