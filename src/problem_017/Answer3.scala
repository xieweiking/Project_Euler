package problem_017

import common._

object Answer3 extends Answer {

    override def title = "Count them by phases."

    def answer = "onethousand".length + (0 /: (0 to 9)) { (sum, h) =>
        sum + (if (h == 0) 0
        else (((if (h == 1) "one"
        else if (h == 2) "two"
        else if (h == 3) "three"
        else if (h == 4) "four"
        else if (h == 5) "five"
        else if (h == 6) "six"
        else if (h == 7) "seven"
        else if (h == 8) "eight"
        else if (h == 9) "nine"
        else "").length + "hundred".length) * 100 + "and".length * 99)) +
            (0 /: (2 to 9)) { (s, d) =>
                s + (if (d == 2) "twenty"
                else if (d == 3) "thirty"
                else if (d == 4) "forty"
                else if (d == 5) "fifty"
                else if (d == 6) "sixty"
                else if (d == 7) "seventy"
                else if (d == 8) "eighty"
                else if (d == 9) "ninety"
                else "").length * 10 +
                    "onetwothreefourfivesixseveneightnine".length
            } +
            "onetwothreefourfivesixseveneightnineteneleventwelvethirteenfourteenfifteensixteenseventeeneighteennineteen".length
    }

}
