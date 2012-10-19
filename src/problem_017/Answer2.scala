package problem_017

import common._

object Answer2 extends Answer {

    override def title = "Not to letters, just to count them."

    def answer = (0 /: (1 to 1000))(_ + countUnderMillion(_))

    def countUnderMillion(x: Int) = x match {
        case outOfMillion if (x >= 1000000 || x < 0) => 0
        case underThousand if (x < 1000)             => countUnderThousand(x)
        case _ =>
            val (underThousand, aboveThousand) = (x % 1000, x / 1000)
            countUnderThousand(aboveThousand) + "thousand".length + (if (underThousand == 0) 0 else countUnderThousand(underThousand))
    }

    def countUnderThousand(x: Int) = x match {
        case outOfThousand if (x >= 1000 || x < 0) => 0
        case underHundred if (x < 100)             => countUnderHundred(x)
        case _ =>
            val (underHundred, aboveHundred) = (x % 100, x / 100)
            countUnderHundred(aboveHundred) + "hundred".length + (if (underHundred == 0) 0 else "and".length + countUnderHundred(underHundred))
    }

    def countUnderHundred(x: Int) = x match {
        case outOfHundred if (x >= 100 || x < 0) => 0
        case underTwenty if (x >= 10 && x < 20)  => countUnderTwenty(x)
        case underTen if (x < 10)                => countUnderTen(x)
        case _ =>
            val count = x / 10 match {
                case 9 => "ninety".length
                case 8 => "eighty".length
                case 7 => "seventy".length
                case 6 => "sixty".length
                case 5 => "fifty".length
                case 4 => "forty".length
                case 3 => "thirty".length
                case 2 => "twenty".length
            }
            val underTen = x % 10
            count + (if (underTen == 0) 0 else countUnderTen(underTen))
    }

    def countUnderTwenty(x: Int) = x match {
        case outOfTwenty if (x >= 20 || x < 0) => 0
        case underTen if (x < 10)              => countUnderTen(x)
        case _ => x % 10 match {
            case 9 => "nineteen".length
            case 8 => "eighteen".length
            case 7 => "seventeen".length
            case 6 => "sixteen".length
            case 5 => "fifteen".length
            case 4 => "fourteen".length
            case 3 => "thirteen".length
            case 2 => "twelve".length
            case 1 => "eleven".length
            case 0 => "ten".length
        }
    }

    def countUnderTen(x: Int) = x match {
        case 9 => "nine".length
        case 8 => "eight".length
        case 7 => "seven".length
        case 6 => "six".length
        case 5 => "five".length
        case 4 => "four".length
        case 3 => "three".length
        case 2 => "two".length
        case 1 => "one".length
        case 0 => "zero".length
        case _ => 0
    }

}
