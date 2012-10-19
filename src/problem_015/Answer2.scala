package problem_015

import common._

object Answer2 extends Answer {

    override def title = "Using formula."

    def answer = routesOfGrid(100)

    def routesOfGrid(scale: Int) = {
        val fact = factorial(scale)
        factorial(scale + scale) / fact / fact
    }

}
