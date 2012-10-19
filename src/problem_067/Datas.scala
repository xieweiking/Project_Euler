package problem_067

import scala.io.Source

object Datas {

    private val source = Source.fromURL(this.getClass.getResource("triangle.txt"))
    val stackStr = source.mkString.trim
    source.close

    def getStack = for (line <- this.stackStr.stripMargin.split("\n|\r\n|\r"))
        yield for (str <- line.split("\\s+"))
        yield (if (str.startsWith("0")) str.substring(1) else str).toInt

}
