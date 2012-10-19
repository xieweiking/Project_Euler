package problem_015

import common._

object Answer3 extends Answer {

    override def title = "Using optimized formula."

    def answer = routesOfGrid(100)

    def routesOfGrid(scale: Int) = combine(scale + scale)(scale)

}
