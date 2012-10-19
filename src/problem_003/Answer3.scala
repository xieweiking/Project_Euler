package problem_003

import common._

object Answer3 extends Answer {

    override def title = "Using BigInt Rabin-Miller & Lucas test."

    def answer = primeFactorsOf(600851475143L, x => BigInt(x abs).isProbablePrime(50)).last

}
