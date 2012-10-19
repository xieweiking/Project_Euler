package problem_024

import common._
import scala.collection.mutable

object Answer1 extends Answer {

    def answer = {
        var chars = ('0' to '9').toArray
        def swap(i: Int, j: Int) {
            val tmp = chars(i)
            chars(i) = chars(j)
            chars(j) = tmp
        }
        val len = chars.length
        val last = len - 1
        var nth = 1
        for (nth <- 2 to 1000000) {
            var max = last
            while (chars(max - 1) >= chars(max)) max -= 1
            val prev = max - 1
            var bottom = last
            while (chars(bottom) <= chars(prev)) bottom -= 1
            swap(prev, bottom)
            bottom = last
            while (max < bottom) {
                swap(max, bottom)
                max += 1
                bottom -= 1
            }
        }
        chars.map(_.toString).reduce(_ + _)
    }

}
