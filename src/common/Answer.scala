package common

trait Answer extends App {

    def title: String = "Brute Force."

    def answer: Any

    private[this] val clazz = this.getClass
    private[this] var problemNumber = clazz.getPackage.getName.substring(8)
    while (problemNumber(0) == '0')
        problemNumber = problemNumber.substring(1)
    private[this] val start = System.nanoTime
    private[this] val a = answer
    private[this] val total = (System.nanoTime - start) / 1000000.0d
    printf(" Answer %s of Problem %s: %s\n     %s\n [Used %s ms]", clazz.getSimpleName.charAt(6), problemNumber, title, a, total)

}
