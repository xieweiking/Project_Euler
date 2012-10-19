package problem_023

import common._
import scala.collection.mutable

object Answer2 extends Answer {
    
    override def title = "Cache All the Abundants."

    def answer = sumOfPerfectAndDeficientNumbers(28123)
    
    def sumOfPerfectAndDeficientNumbers(max: Int) = {
        val abundants = new mutable.ArrayBuffer[Int]()
        val sumOf2Abundants = new Array[Boolean](max)
        val range = 1 until max
        for (n  <- range if isAbundant(n))
            abundants += n
        val size = abundants.size
        for {
            i <- 0 until size
            j <- i until size
        } {
            val idx = abundants(i) + abundants(j)
            if (idx < max)
                sumOf2Abundants(idx) = true
        }
        var sum = 0
        for (n <- range if !sumOf2Abundants(n))
            sum += n
        sum
    }

    @inline
    def isAbundant(x: Int) = x < sumOfDivisors(x)

}