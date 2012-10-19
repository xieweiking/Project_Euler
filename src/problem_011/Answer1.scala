package problem_011

import common._

object Answer1 extends Answer {

    def answer = {
        var product = 0
        for {
            i <- 0 to 19
            j <- 0 to 19
        } {
            if (i <= 16) {
                product = product max productOfVertical(i, j)
                if (j <= 16) product = product max productOfDiagonal(i, j)
            }
            if (j <= 16) {
                product = product max productOfHorizontal(i, j)
                if (i >= 3) product = product max prodectOfBackDiagonal(i, j)
            }
        }
        product
    }

    def productOfHorizontal(i: Int, j: Int) =
        Datas.grid(i)(j) * Datas.grid(i)(j + 1) * Datas.grid(i)(j + 2) * Datas.grid(i)(j + 3)

    def productOfVertical(i: Int, j: Int) =
        Datas.grid(i)(j) * Datas.grid(i + 1)(j) * Datas.grid(i + 2)(j) * Datas.grid(i + 3)(j)

    def productOfDiagonal(i: Int, j: Int) =
        Datas.grid(i)(j) * Datas.grid(i + 1)(j + 1) * Datas.grid(i + 2)(j + 2) * Datas.grid(i + 3)(j + 3)

    def prodectOfBackDiagonal(i: Int, j: Int) =
        Datas.grid(i)(j) * Datas.grid(i - 1)(j + 1) * Datas.grid(i - 2)(j + 2) * Datas.grid(i - 3)(j + 3)

}
