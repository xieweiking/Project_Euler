package problem_018

import common._
import scala.collection.mutable._

object Answer1 extends Answer {

    def answer = {
        val record = new ArrayBuffer[(Int, String)](math.pow(2, Datas.stack.length - 1).toInt)
        deepFirstTraverse(record)
        val result = record.max
        result._1 + " = " + result._2
    }

    def deepFirstTraverse(record: ArrayBuffer[(Int, String)], path: Stack[Int] = Stack(), row: Int = 0, col: Int = 0) {
        if (row < Datas.stack.length) {
            val r = Datas.stack(row)
            if (col < r.length) {
                path.push(Datas.stack(row)(col))
                if (row == Datas.stack.length - 1)
                    record.append((path.sum, path.reverseIterator.map(_.toString).reduce(_ + " + " + _)))
                else {
                    val nextRow = row + 1
                    deepFirstTraverse(record, path, nextRow, col)
                    deepFirstTraverse(record, path, nextRow, col + 1)
                }
                path.pop
            }
        }
    }

}
