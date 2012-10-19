package problem_023

import common._
import scala.util.control.Breaks._

object Answer1 extends Answer {

    def answer = sumOfPerfectAndDeficientNumbers(28123)
    
    def sumOfPerfectAndDeficientNumbers(max: Int) = {
        var sum = (1L to 23L).sum
        for (num <- 25 to max if num % 12 != 0) {
            var notFound = true
            breakable {
                for (i <- 12 until num - 12 if isAbundant(i) && isAbundant(num - i)) {
                    notFound = false
                    break
                }
            }
            if (notFound)
                sum += num
        }
        sum
    }

    @inline
    def isAbundant(x: Int) = x < sumOfDivisors(x)

}