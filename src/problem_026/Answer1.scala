package problem_026

import common._
import scala.collection.mutable

object Answer1 extends Answer {

    def answer = {
        var (max, d) = (1, 3)
        for (num <- d until 1000) {
            val lenth = recurringCycleLengthInUnitFraction(num)
            if (lenth > max) {
                max = lenth
                d = num
            }
        }
        d
    }

    def recurringCycleLengthInUnitFraction(num: Int, printResult: Boolean = false) = {
        val quotients = mutable.ArrayBuffer[Int]()
        val remainders = mutable.ArrayBuffer(1)
        val borrows = mutable.ArrayBuffer(1)
        var (nextScale, offset) = (10, 0)
        while (nextScale < num) {
            remainders(0) *= 10
            borrows += nextScale
            nextScale *= 10
            offset += 1
        }
        var (notFoundRecurringCycle, recurringCycleIndex) = (true, -1)
        while (remainders.last != 0 && notFoundRecurringCycle) {
            var divident = 10 * remainders.last
            while (divident < num) {
                divident *= 10
                quotients += 0
                remainders += -1
            }
            quotients += divident / num
            val remainder = divident % num
            val borrow = borrows.indexOf(remainder)
            if (borrow != -1) {
                recurringCycleIndex = -offset + borrow
                notFoundRecurringCycle = false
            }
            else {
                recurringCycleIndex = remainders.indexOf(remainder)
                notFoundRecurringCycle = recurringCycleIndex == -1
                if (notFoundRecurringCycle)
                    remainders += remainder
            }
        }
        val hasRecurringCycle = !notFoundRecurringCycle
        val recurringCycleLength = if (hasRecurringCycle) quotients.length - recurringCycleIndex else 0
        if (printResult) {
            printf("1/%d = 0.", num)
            while (offset > 0) {
                if (hasRecurringCycle && -recurringCycleIndex == offset) print('(')
                print('0')
                offset -= 1
            }
            for (i <- 0 until quotients.length) {
                if (hasRecurringCycle && i == recurringCycleIndex) print('(')
                print(quotients(i))
            }
            if (hasRecurringCycle)
                printf("): %d", recurringCycleLength)
            println
        }
        recurringCycleLength
    }

}
