import scala.annotation.tailrec

// list of problems geared towards Scala curated from https://kkspeed.github.io/leetcode-scala/
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
  // https://leetcode.com/problems/same-tree/description/
  case class TreeNode(_value: Int = 0, _left: TreeNode, _right: TreeNode) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
    def toList: List[Int] = {
      List(_value) ++ _left.toList ++ _right.toList
    }
  }
  def isSameTree(p: TreeNode, q: TreeNode): Boolean = {
    if (p == null && q == null) true
    else if (p == null || q == null) false
    else
      p._value == q._value && isSameTree(p._left, q._left) && isSameTree(
        p._right,
        q._right
      )
  }
  //https://leetcode.com/problems/symmetric-tree/description/
  def isSymmetric(root: TreeNode): Boolean = {

    def isMirrored(left: TreeNode, right: TreeNode): Boolean = {
      if (left == null && right == null) true
      else if (left == null || right == null) false
      else
        left.value == right.value && isMirrored(left.right, right.left) && isMirrored(
          left.left,
          right.right
        )
    }
    if (root == null) true
    else isMirrored(root._left, root._right)
  }
// https://leetcode.com/problems/find-permutation/description/
  def findPermutation(s: String): Array[Int] = {}

  // convert to tail recursion
  def poorSum(n: Int): Int =
    if (n == 0) 0
    else n + poorSum(n - 1)

  def betterSum(n: Int): Int = {
    @tailrec
    def loop(i: Int, acc: Int): Int = {
      if (i == 0) acc
      else loop(i - 1, acc + i)
    }
    loop(n, 0)
  }
  // https://leetcode.com/problems/bulb-switcher/description/
  // this involves discovering the algorithm underlying by detecting the series pattern

  def bulbSwitch(n: Int): Int = {}
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
  // convert to list
  // zipWithIndex
  // add zero to beginning and end

}
