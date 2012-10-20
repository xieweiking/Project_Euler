package main

object Main extends App {

    var more = true
    var i = 0
    val arg = Array.empty[String]
    do {
        i += 1
        try {
            val clazzName = "problem_%03d.package".format(i)
            val problem = Class.forName(clazzName)
            if (i > 1) println("\n\n")
            problem.getMethod("main", arg.getClass).invoke(problem, arg)
        }
        catch {
            case _: ClassNotFoundException => more = false
        }
    } while (more)

}
