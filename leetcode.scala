import scala.annotation.tailrec

// list of problems geared towards Scala curated from https://kkspeed.github.io/leetcode-scala/
// goal is to solve ,using fp solutions. 
// problems designed in non FP terms , such as nullable etc.. will be converted 
object leetcode extends App {
  // https://leetcode.com/problems/sqrtx/description/
  def mySqrt(x: Int): Int = {
    def loop(guess: Double): Double = {
      if (Math.abs(guess * guess - x) < 0.001) guess
      else
        loop((guess + x / guess) / 2)
    }
    loop(2).toInt
  }
  //https://leetcode.com/problems/ugly-number/description/
  def isUgly(n: Int): Boolean = {
    def loop(n: Int): Boolean = {
      if (n <= 0) false
      else if (n == 1) true
      else if (n % 2 == 0) loop(n / 2)
      else if (n % 3 == 0) loop(n / 3)
      else if (n % 5 == 0) loop(n / 5)
      else false
    }
    loop(n)
  }

// https://leetcode.com/problems/valid-parentheses/description/
  val markers = Map('(' -> ')', '[' -> ']', '{' -> '}')
  def isValid(s: String): Boolean = {
    def loop(value: List[Char], stack: List[Char]): Boolean = {
      value match {
        case ::(curr, strRemainder) =>
          if (markers.contains(curr)) loop(strRemainder, curr :: stack)
          else
            stack match {
              case ::(head, stackRemainder) =>
                markers.get(head).exists(_.==(curr)) && loop(
                  strRemainder,
                  stackRemainder
                )
              case Nil => false
            }
        case Nil => stack.isEmpty
      }
    }

      loop(s.toCharArray.toList, List.empty[Char])

    }
  }
def addTwoNumbers(l1: List[Int], l2: List[Int]): List[Int] = {
    def convertToNumber(list: List[Int]): Int = {
      list
        .foldRight[(Int, Int)]((0, 1)) {
          case (elem, (acc, multiplier)) =>
            (acc + (elem * multiplier), multiplier * 10)
        }
        ._1
    }
    val res = convertToNumber(l1) + convertToNumber(l2)
    List.unfold(res)(i => if (i > 0) Some(i % 10, i / 10) else None)

  }


}
