package problem_028

import common.Answer

object Answer1 extends Answer {

    def answer = {
        val scale = 5
        val half = scale / 2
        val diagonalRange = -half to half
        val spiral = new Spiral(scale)
        var sum = 0
        for (i <- diagonalRange)
            sum += spiral(i, i)
        for (i <- diagonalRange if i != 0)
            sum += spiral(i, -i)
        sum
    }

    class Spiral(scale: Int) {

        def apply(x: Int, y: Int) = {
            1
        }

    }

}
