import scala.util.continuations.{ reset, shift, cpsParam }
import scala.annotation.tailrec
import scala.collection.mutable
import java.util.Arrays
import scala.language.reflectiveCalls

package object common {

    type cps[A] = cpsParam[A, A]

    /**
     * Use Stream to represent any infinite sequence.
     */
    @inline
    final def trampoline[R](body: => Iteration[R]) = {
        def loop(thunk: () => Iteration[R]): Stream[R] = thunk apply match {
            case Yield(result, next) => Stream.cons(result, loop(next))
            case Done                => Stream.empty  // Using List sometime will get stack over-flow.
        }
        loop(() => body).iterator
    }

    /**
     * The generator decoration, in order to use yield_return.
     */
    final def generator[R](body: => Unit @cps[Iteration[R]]) = trampoline[R](reset { body; Done })

    /**
     * Can be used in while loop ONLY, 'cause of the cps problem.
     */
    @inline
    final def yield_return[R](result: R): Unit @cps[Iteration[R]] = shift((k: Unit => Iteration[R]) => Yield(result, () => k()))

    def lcm(a: Long, b: Long) = a * b / gcd(a, b)

    @tailrec
    final def gcd(a: Long, b: Long): Long =
        if (b == 0) a
        else gcd(b, a % b)

    final def primeFactorsOf(x: Long, primeTest: Long => Boolean = isPrime, p: Long = 2): List[Long] = {
        val num = x.abs
        if (primeTest(num)) List(num)
        else {
            var f = p
            do { // can NOT use for each loop, 'cause return value from it will slow down or cause problem.
                if (f > 5 && f % 5 == 0)
                    f += 2
                if (primeTest(f) && num % f == 0)
                    return f :: primeFactorsOf(num / f, primeTest, f)
                if (isEven(f)) f += 1
                else f += 2
            } while (f <= num)
            List(p)
        }
    }

    @inline
    final def isPrime(x: Long): Boolean = {
        val num = x.abs
        val str = num.toString
        if (num == 2 || num == 3 || num == 5 || num == 7 || num == 11) true
        else if (num < 2 || isEven(num) || (str.map(_ - '0').reduce(_ + _)) % 3 == 0 || str.endsWith("5") || num % 7 == 0) false
        else { // can NOT use for each loop, 'cause return value from it will slow down or cause problem.
            var odd = 11L
            val end = math.sqrt(num).asInstanceOf[Long] + 1
            while (odd <= end) {
                if (num % odd == 0)
                    return false
                odd += 2
            }
            true
        }
    }

//    @inline
//    final def divisorsOf(n: Long, primeTest: Long => Boolean = isPrime) =
//        if (n <= 1L) Set[Long]()
//        else if (primeTest(n)) Set(1L)
//        else {
//            val divisors = mutable.Set(1L)
//            val factorMap = mutable.Map[Long, Int]()
//            for (f <- primeFactorsOf(n, primeTest)) {
//                if (!factorMap.contains(f)) {
//                    factorMap(f) = 1
//                    divisors += f
//                }
//                else {
//                    factorMap(f) = factorMap(f) + 1
//                    var prod = 1L
//                    for ((f, p) <- factorMap)
//                        prod *= math.pow(f, p).longValue
//                    divisors += prod
//                }
//            }
//            divisors
//        }

    @inline
    final def sumOfDivisors(n: Int) =
        if (n <= 1) 0
        else if (n == 2) 1
        else {
            var sum = 1
            for (divisor <- 2 to math.sqrt(n).intValue if n % divisor == 0) {
                sum += divisor
                val other = n / divisor
                if (divisor < other)
                    sum += other
            }
            sum
        }

    @inline
    final def ln(x: Double) = math.log(x)

    @inline
    final def logN(n: Double, x: Double) = math.log(x) / math.log(n)

    @inline
    final def log10(x: Double) = logN(10, x)

    @inline
    final def log2(x: Double) = logN(2, x)

    @inline
    final def sumOfArithmeticSeq(a1: Long, delta: Long, max: Long) =
        if (a1 > max) 0
        else {
            val n = (max - a1) / delta + 1
            val an = a1 + (n - 1) * delta
            (a1 + an) * n / 2
        }

    @inline
    final def sumOfGeometricSeq(a1: Long, q: Double, n: Long) =
        if (q == 1.0) n * a1
        else (a1 - a1 * math.pow(q, n)) / (1 - q)

    @inline
    final def isEven(x: Int) = (x & 1) == 0

    @inline
    final def isOdd(x: Int) = !isEven(x)

    @inline
    final def isEven(x: Long) = (x & 1L) == 0

    @inline
    final def isOdd(x: Long) = !isEven(x)

    @inline
    final def isEven(x: BigInt) = (x & BigInt(1)) == 0

    @inline
    final def isOdd(x: BigInt) = !isEven(x)

    @inline
    final def isInteger(x: Double) = x.asInstanceOf[Int].asInstanceOf[Double] == x

    @inline
    final def isLong(x: Double) = x.asInstanceOf[Long].asInstanceOf[Double] == x

    @inline
    final def isSquare(n: Int) = {
        var i = 1
        var next = n
        while (next > 0) {
            next -= i
            i += 2
        }
        0 == next
    }

    final def factorial(x: Int): BigInt =
        if (x <= 1) 1
        else x * factorial(x - 1)

    final def arrange(total: Int)(option: Int): BigInt =
        if (option > total || total == 0) 0
        else if (option == total || option == 0) 1
        else if (option < total / 2) arrange(total)(total - option)
        else total * arrange(total - 1)(option)

    final def combine(total: Int)(option: Int) = arrange(total)(option) / factorial(option)

    @inline
    final def arrangeAny(total: Int)(min: Int, max: Int) = sumOfAny(total)(min, max)(arrange(_)(_))

    @inline
    final def combineAny(total: Int)(min: Int, max: Int) = sumOfAny(total)(min, max)(combine(_)(_))

    @inline
    final def arrangeAll(total: Int)(max: Int) = arrangeAny(total)(1, max)

    @inline
    final def combineAll(total: Int)(max: Int) = combineAny(total)(1, max)

    @inline
    final def powMod(a: Long, b: Long, m: Long) = {
        var r = 1L
        var (rA, rB) = (a % m, b)
        while (rB > 0) {
            if ((rB & 1) != 0) r = abMod(r, rA, m)
            rA = abMod(rA, rA, m)
            rB = rB >> 1
        }
        r
    }

    @inline
    final def abMod(a: Long, b: Long, m: Long) = {
        var (rA, rB) = (a % m, b % m)
        var r = 0L
        while (rB > 0) {
            if ((rB & 1) != 0) r = (r + rA) % m
            rA = (rA << 1) % m
            rB = rB >> 1
        }
        r
    }

    @inline
    final def nthOdd(n: Int) = if (n <= 0) 1 else {
        2 * n + 1
    }

    @inline
    final def nthEven(n: Int) = if (n <= 0) 0 else {
        2 * n
    }

    @inline
    private final def sumOfAny(total: Int)(min: Int, max: Int)(calc: (Int, Int) => BigInt): BigInt =
        if (min > max || min > total) 0
        else {
            var (sum, i) = (BigInt(0), min)
            while (i <= max && i <= total) {
                sum += calc(total, i)
                i += 1
            }
            sum
        }

    final val CONCURRENT_COUNT = 2 * Runtime.getRuntime.availableProcessors

    final def using[R, T <: { def close(): Any }](closable: T)(block: T => R): R = {
        try {
            block(closable)
        }
        finally {
            closable.close()
        }
    } 
    
}
