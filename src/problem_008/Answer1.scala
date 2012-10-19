package problem_008

import common.Answer

object Answer1 extends Answer {

    def answer = {
        val datas = Datas.seq
        var max, i = 0
        val last = datas.size - 5
        while (i < last) {
            val tmp = datas(i) * datas(i + 1) * datas(i + 2) * datas(i + 3) * datas(i + 4)
            if (tmp > max) max = tmp
            i += 1
        }
        max
    }

}
