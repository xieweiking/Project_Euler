package problem_067

import common._
import scala.io.Source

object Datas {

    val stackStr = using(Source.fromURL(this.getClass.getResource("triangle.txt"))) { source =>
        source.mkString.trim
    }

    def getStack = for (line <- this.stackStr.stripMargin.split("\n|\r\n|\r"))
        yield for (str <- line.split("\\s+"))
        yield (if (str.startsWith("0")) str.substring(1) else str).toInt

}
