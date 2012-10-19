package problem_017

import common._

object Answer1 extends Answer {

    def answer = (0 /: (1 to 1000)) { (sum, i) =>
        sum + countLetters(numberLetters(i))
    }

    def countLetters(str: String) = (0 /: str) { (sum, ch) =>
        sum + (if ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')) 1 else 0)
    }

    def numberLetters(x: Long) =
        lettersUnderBound(x)(1000000000000000000L)("pillion") {
            lettersUnderBound(_)(1000000000000000L)("tillion") {
                lettersUnderBound(_)(1000000000000L)("billion") {
                    lettersUnderBound(_)(1000000000L)("million") {
                        lettersUnderBound(_)(1000000L)("thousand", " ") {
                            lettersUnderBound(_)(1000, 100)("hundred", " and ")(lettersUnderHundred)
                        }
                    }
                }
            }
        }

    def lettersUnderBound(x: Long)(bound: Long, lastBound: Long = 0)(unitStr: String, sep: String = ", ")(lettersOfLastBound: Long => String) =
        x match {
            case outOfBound if (x >= bound || x < 0)                                      => ""
            case underLastBound if ((lastBound > 0 && x < lastBound) || x < bound / 1000) => lettersOfLastBound(x)
            case _ =>
                val lastB = if (lastBound > 0) lastBound else bound / 1000
                val (underLastBound, aboveLastBound) = (x % lastB, x / lastB)
                lettersOfLastBound(aboveLastBound) + " " + unitStr + (if (underLastBound == 0) "" else sep + lettersOfLastBound(underLastBound))
        }

    def lettersUnderHundred(x: Long) = x match {
        case outOfHundred if (x >= 100 || x < 0) => ""
        case underTwenty if (x >= 10 && x < 20)  => lettersUnderTwenty(x)
        case underTen if (x < 10)                => lettersUnderTen(x)
        case _ =>
            val str = x / 10 match {
                case 9 => "ninety"
                case 8 => "eighty"
                case 7 => "seventy"
                case 6 => "sixty"
                case 5 => "fifty"
                case 4 => "forty"
                case 3 => "thirty"
                case 2 => "twenty"
            }
            val underTen = x % 10
            str + (if (underTen == 0) "" else "-" + lettersUnderTen(underTen))
    }

    def lettersUnderTwenty(x: Long) = x match {
        case outOfTwenty if (x >= 20 || x < 0) => ""
        case underTen if (x < 10)              => lettersUnderTen(x)
        case _ => x % 10 match {
            case 9 => "nineteen"
            case 8 => "eighteen"
            case 7 => "seventeen"
            case 6 => "sixteen"
            case 5 => "fifteen"
            case 4 => "fourteen"
            case 3 => "thirteen"
            case 2 => "twelve"
            case 1 => "eleven"
            case 0 => "ten"
        }
    }

    def lettersUnderTen(x: Long) = x match {
        case outOfTen if (x >= 10 || x < 0) => ""
        case 9                              => "nine"
        case 8                              => "eight"
        case 7                              => "seven"
        case 6                              => "six"
        case 5                              => "five"
        case 4                              => "four"
        case 3                              => "three"
        case 2                              => "two"
        case 1                              => "one"
        case 0                              => "zero"
    }

}
