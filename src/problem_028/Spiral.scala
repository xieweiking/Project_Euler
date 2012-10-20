package problem_028

import common._

class Spiral(scale: Int) {

    require(isOdd(scale) && scale > 0)

    private lazy val halfScale = scale / 2
    private lazy val sqaureScale = scale * scale

    def diagonalPositions = generator {
        yield_return(0, 0)
        var s = 1
        while (s <= this.halfScale) {
            yield_return(s, -s)
            yield_return(-s, -s)
            yield_return(-s, s)
            yield_return(s, s)
            s += 1
        }
    }

    def numberAt(x: Int, y: Int) = {
        val (absX, absY) = (x.abs, y.abs)
        require(absX <= this.halfScale && absY <= this.halfScale)
        val level = absX max absY
        val (max, edge) = Spiral.maxAndEdgeOf(level)
        if (x == y) {
            if (x >= 0) max
            else max - 2 * edge
        }
        else if (absX == absY) {
            if (x < 0) max - edge
            else max - 3 * edge
        }
        else if (y < 0 && x - 1 == y) edge * edge
        else {
            val delta = (level - x) + (level - y)
            if (x < y) max - delta
            else max - (4 * edge - delta)
        }
    }

    def numberAt(pos: (Int, Int)): Int = this.numberAt(pos._1, pos._2)

    def positionOf(n: Int) = {
        require(n >= 1 && n <= this.sqaureScale)
        if (n == 1)
            (0, 0)
        else if (n == this.sqaureScale)
            (this.halfScale, this.halfScale)
        else {
            val sqrtN = math.sqrt(n)
            val intSqrtN = sqrtN.toInt
            if (isSquare(n)) {
                if (isOdd(n)) {
                    val level = (intSqrtN - 1) / 2
                    (level, level)
                }
                else {
                    val y = -intSqrtN / 2
                    (y + 1, y)
                }
            }
            else if (isEven(intSqrtN)) {
                val level = intSqrtN / 2
                val (max, edge) = Spiral.maxAndEdgeOf(level)
                val top = max - edge
                val delta = n - top
                if (delta >= 0)
                    (-level + delta, level)
                else
                    (-level, level + delta)
            }
            else {
                val edge = intSqrtN + 1
                val max = edge * edge + 1
                val level = edge / 2
                val bottom = max - edge
                val delta = n - bottom
                if (delta >= 0)
                    (level - delta, -level)
                else
                    (level, -level - delta)
            }
        }
    }

    def apply(n: Int) = this.positionOf(n)

}

object Spiral {

    def apply(scale: Int) = new Spiral(scale)

    def maxAndEdgeOf(level: Int) = {
        require(level >= 0)
        val o = nthOdd(level)
        (o * o, o - 1)
    }

}
