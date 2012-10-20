package common

trait Problem extends App {

    def question: String

    println("===================================================================================================")
    println(" " + this.getClass.getPackage.getName.replace('_', ' ').toUpperCase + ":\n")
    println(question.stripMargin)
    println("---------------------------------------------------------------------------------------------------")
    val prefix = this.getClass().getPackage().getName() + ".Answer"
    var more = true
    var i = 0
    do {
        i += 1
        try {
            val answer = Class.forName(prefix + i)
            val arg = Array[String]()
            println
            answer.getMethod("main", arg.getClass).invoke(answer, arg)
            println
        }
        catch {
            case _: ClassNotFoundException => more = false
        }
    } while (more)
    print("\n===================================================================================================")

}
