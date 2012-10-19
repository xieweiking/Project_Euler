package problem_019

import common._

object Answer2 extends Answer {

    override def title = "Direct accumulate SUNDAY."

    def answer = {
        var (year, count, accumulate) = (1900, 0, 1)
        while (year <= 2000) {
            var month = 1
            while (month <= 12) {
                if (year >= 1901 && accumulate % 7 == 0)
                    count += 1
                accumulate +=
                    (if (month == 2) (if (if (year % 100 == 0) year % 400 == 0 else year % 4 == 0) 29 else 28)
                    else if (month == 2 || month == 4 || month == 6 || month == 9 || month == 11) 30
                    else 31)
                month += 1
            }
            year += 1
        }
        count
    }

}
