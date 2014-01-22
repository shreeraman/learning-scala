package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
      def balance(parenthesisCount: Int, chars: List[Char]): Boolean = {
        if (parenthesisCount < 0) {
          false
        } else if (chars.isEmpty) {
          !(parenthesisCount > 0)
        } else {
          val headChar = chars.head
          if (headChar == '(') balance(parenthesisCount + 1, chars.tail)
          else if (headChar == ')') balance(parenthesisCount - 1, chars.tail)
          else balance(parenthesisCount, chars.tail)
        }
      }

    balance(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else {
      if (coins.isEmpty) 0
      else {
        val headCoin = coins.head
        if (headCoin > money) countChange(money, coins.tail)
        else countChange(money - headCoin, coins) + countChange(money, coins.tail)
      }
    }
  }
}
