package problem_022

import common._

object Answer1 extends Answer {

    def answer = {
        val names = Datas.names.sorted
        var i, total = 0
        while (i < names.length) {
            val name = names(i)
            i += 1
            total += name.map(_ - 'A' + 1).sum * i
        }
        total
    }

}
