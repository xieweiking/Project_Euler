package problem_015

import common._

object Answer1 extends Answer {

    def answer = routeFactorsOfGrid(100)._1

    def routeFactorsOfGrid(scale: BigInt): (BigInt, Array[BigInt]) =
        if (scale < 1) (1L, Array.empty)
        else {
            val pre = routeFactorsOfGrid(scale - 1)
            val preAry = pre._2
            val preAryLen = preAry.length;
            val array = new Array[BigInt](preAryLen + 1)
            array(0) = pre._1
            var (sum, i) = (pre._1, 1)
            while (i <= preAryLen) {
                var (subSum, j) = (BigInt(0), i - 1)
                while (j < preAryLen) {
                    subSum += preAry(j)
                    j += 1
                }
                array(i) = subSum
                sum += subSum
                i += 1
            }
            (2 * sum, array)
        }

}
