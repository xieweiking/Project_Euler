package problem_020

import common._

object Answer1 extends Answer {

    def answer = factorial(100).toString.map(_ - '0').reduce(_ + _)

}