package problem_028

import common._

object Answer1 extends Answer {

    override def title = "Position to spiral number."

    def answer = {
        val spiral = Spiral(1001)
        (0 /: spiral.diagonalPositions) { (sum, pos) =>
            sum + spiral(pos._1, pos._2)
        }
    }

}
