package problem_021

import common._
import scala.collection.mutable

object Answer1 extends Answer {

    def answer = {
        val hitSet = mutable.Set[Int]()
        for (i <- 1 until 10000 if !hitSet.contains(i)) {
            val x = d(i)
            if (i != x && d(x) == i) {
                hitSet += i
                hitSet += x
            }
        }
        hitSet.sum
    }

    def d(n: Int) = sumOfDivisors(n)

}
