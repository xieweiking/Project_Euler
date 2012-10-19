package problem_022

import scala.io.Source

object Datas {

    private val source = Source.fromURL(this.getClass.getResource("names.txt"))
    val namesStr = source.mkString.trim
    source.close

    val names = namesStr.substring(1, namesStr.length - 1).split("\",\"")

}