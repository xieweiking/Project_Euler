package problem_028

object TestSpiral extends App {

    def test(n: Int) = {
        val maxLevel = (n - 1) / 2
        val s = Spiral(n)
        for {
            y <- maxLevel to -maxLevel by -1
            x <- -maxLevel to maxLevel
        } {
            val number = s.numberAt(x, y)
            val fact = s(number)
            if (fact != (x, y))
                throw new IllegalStateException("'%s' was expacted at %s, but now is at %s!".format(number, (x, y), fact))
        }
        println("Class Spiral is Parfait~!")
    }

    test(1001)

}
