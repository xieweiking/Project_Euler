package main

object Main extends App {

    private def toThreeDigitStr(index: Int) = {
        var str = index.toString
        val len = str.length
        if (len == 1)
            "00" + str
        else if (len == 2)
            "0" + str
        else
            str
    }

    var more = true
    var i = 0
    val arg = Array[String]()
    do {
        i += 1
        try {
            val problem = Class.forName(String.format("problem_%s.package", toThreeDigitStr(i)))
            if (i > 1) println("\n\n")
            problem.getMethod("main", arg.getClass).invoke(problem, arg)
        }
        catch {
            case _: ClassNotFoundException => more = false
        }
    } while (more)

}
