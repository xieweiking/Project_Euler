package problem_028

import common._

class Spiral(scale: Int) {

    require(isOdd(scale) && scale > 0)

    private lazy val half = scale / 2

    def diagonalPositions = generator {
        yield_return(0, 0)
        var s = 1
        while (s <= this.half) {
            yield_return( s, -s)
            yield_return(-s, -s)
            yield_return(-s,  s)
            yield_return( s,  s)
            s += 1
        }
    }

    def apply(x: Int, y: Int) = {
        val (absX, absY) = (x.abs, y.abs)
        require(absX <= this.half && absY <= this.half)
        val (max, edge) = Spiral.maxAndEdgeOfLevel(absX max absY)
        if (x == y) {
            if (x >= 0) max
            else max - 2 * edge // math.pow(even(absY), 2)
        }
        else if (absX == absY) {
            if (x < 0) max - edge
            else max - 3 * edge
        }
        else {
            // TODO
            0
        }
    }

    def apply(index: Int) = {
        require(index >= 1)
        // TODO
        (0, 0)
    }

}

object Spiral {

    def apply(scale: Int) = new Spiral(scale)

    def maxAndEdgeOfLevel(n: Int) = {
        require(n >= 0)
        val o = nthOdd(n)
        (o * o, o - 1)
    }

}
