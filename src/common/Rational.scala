package common

class Rational(n: Int, d: Int) {

    require(d != 0)

    private[this] val g = gcd(n abs, d abs).asInstanceOf[Int]
    val numer = n / g
    val denom = d / g

    def this(n: Int) = this(n, 1)

    def +(that: Rational) = Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom
    )

    def -(that: Rational) = Rational(
        numer * that.denom - that.numer * denom,
        denom * that.denom
    )

    def -(i: Int) = Rational(
        numer - i * denom,
        denom
    )

    def *(that: Rational) = Rational(
        numer * that.numer,
        denom * that.denom
    )

    def *(i: Int) = Rational(
        numer * i,
        denom
    )

    def /(that: Rational) = Rational(
        numer * that.denom,
        denom * that.numer
    )

    def /(i: Int) = Rational(
        numer,
        denom * i
    )

    def <(that: Rational) = numer * that.denom < that.numer * denom

    def max(that: Rational) = if (this < that) that else this

    override def toString = numer + "/" + denom

}

object Rational extends App {

    def max(r0: Rational, r1: Rational, rs: Rational*) = {
        val list = Array(r0, r1, rs)

    }
    println(Rational(1, 2) < Rational(1, 3))

    def apply(n: Int, d: Int) = new Rational(n, d)

    def apply(n: Int) = new Rational(n)

}
