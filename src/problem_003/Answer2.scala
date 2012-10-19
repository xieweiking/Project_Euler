package problem_003

import common._

object Answer2 extends Answer {

    override def title = "Using odd Prime divide test."

    def answer = primeFactorsOf(600851475143L).last

}
