package problem_018

import common._

object Answer2 extends Answer {

    override def title = "Roll the sum from bottom."

    def answer = Solution.getExpr(for (row <- Datas.stack) yield for (num <- row) yield num)

    object Solution {

        def getExpr(stack: Array[Array[Int]]) = {
            val paths = for (row <- stack) yield for (num <- row) yield List(num.toString)
            if (stack.length > 1) {
                var rowIdx = stack.length - 1;
                while (rowIdx > 0) {
                    val prevRowIdx = rowIdx - 1;
                    val (prevRow, row) = (stack(prevRowIdx), stack(rowIdx))
                    val (prevPath, path) = (paths(prevRowIdx), paths(rowIdx))
                    var col = 0
                    while (col < prevRow.length) {
                        val next = col + 1
                        val idx = if (row(col) > row(next)) col else next
                        prevRow(col) += row(idx)
                        prevPath(col) ++= path(idx)
                        col = next
                    }
                    rowIdx = prevRowIdx
                }
            }
            stack(0)(0) + " = " + paths(0)(0).reduce(_ + " + " + _)
        }

    }

}
