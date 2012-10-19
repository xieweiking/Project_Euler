package problem_009

import common._

object Answer1 extends Answer {

    def answer: Int = {
        var b = 1
        while (b < 1000) {
            var a = 1
            while (a < b) {
                val c = 1000 - a - b
                if (c > b && a * a + b * b == c * c) {
                    return a * b * c
                }
                a += 1
            }
            b += 1
        }
        0
    }

}
