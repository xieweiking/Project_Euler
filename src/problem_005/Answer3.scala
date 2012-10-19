package problem_005

import common._
import scala.collection.mutable

object Answer3 extends Answer {

    override def title = "Using max prime factor power multiple."

    def answer = {
        val primeCount = mutable.Map[Long, Int]()
        for (i <- 2 to 20) populateMaxCount(countFactors(primeFactorsOf(i)), primeCount)
        (for ((p, c) <- primeCount) yield math.pow(p, c).asInstanceOf[Long]).reduce(_ * _)
    }

    def populateMaxCount(src: mutable.Map[Long, Int], dest: mutable.Map[Long, Int]) = for ((prime, count) <- src)
        if (!dest.contains(prime)) dest += (prime -> count)
        else if (dest(prime) < count) dest(prime) = count

    def countFactors(factors: List[Long]) = {
        val factorCount = mutable.Map[Long, Int]()
        for (f <- factors)
            if (factorCount contains f) factorCount(f) += 1
            else factorCount += (f -> 1)
        factorCount
    }

}
