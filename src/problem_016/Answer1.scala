package problem_016

import common._

object Answer1 extends Answer {

    override def title = "Using BigInt."

    def answer = {
        val bytes = new Array[Byte](126)
        bytes(0) = 0x01.toByte
        (0 /: BigInt(bytes).toString)(_ + _ - '0')
    }

}
