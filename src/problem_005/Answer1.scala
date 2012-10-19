package problem_005

import common._

object Answer1 extends Answer {

    override def title = "Using LCM."

    def answer = ((1 to 20) :\ 1L)(lcm(_, _))

}
