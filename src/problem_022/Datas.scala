package problem_022

import common._
import scala.io.Source

object Datas {

    val namesStr = using(Source.fromURL(this.getClass.getResource("names.txt"))) { source =>
        source.mkString.trim
    }

    val names = namesStr.substring(1, namesStr.length - 1).split("\",\"")

}