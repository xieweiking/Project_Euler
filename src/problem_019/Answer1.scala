package problem_019

import common._
import java.util.Calendar

object Answer1 extends Answer {

    override def title = "Brute Force By Using Calendar API."

    def answer = {
        val cal = Calendar.getInstance
        var (year, count) = (1901, 0)
        while (year <= 2000) {
            var month = Calendar.JANUARY
            while (month <= Calendar.DECEMBER) {
                cal.set(year, month, 1)
                if (cal.get(Calendar.DAY_OF_WEEK) == Calendar.SUNDAY)
                    count += 1
                month += 1
            }
            year += 1
        }
        count
    }

}
