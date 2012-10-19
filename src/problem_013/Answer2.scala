package problem_013

import common._

object Answer2 extends Answer {

    override def title = "Emulating human addition."

    def answer = {
        var carry = 0
        val summation = Range(Datas.grid(0).size - 1, -1, -1) map { col =>
            val sum = (carry /: (0 until Datas.grid.size))((s, row) => s + Datas.grid(row)(col))
            carry = sum / 10
            sum % 10
        }
        val (start, diff) = if (carry > 0) {
            val str = carry.toString
            (str, 10 - str.size)
        }
        else ("", 10)
        start + summation.slice(summation.size - diff, summation.size).reverse.mkString
    }

}
