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

    def balance(chars: List[Char], beginNumber: Int): Boolean = {

      if (beginNumber == 0 && chars.isEmpty) true
      else {
        if (beginNumber < 0) false
        else {
          if (beginNumber > 0 && chars.isEmpty) false
          else {
            if (chars.head == '(') balance(chars.tail, beginNumber + 1)
            else {
              if (chars.head == ')') balance(chars.tail, beginNumber - 1)
              else balance(chars.tail, beginNumber)
            }
          }
        }
      }
    }

    balance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    if (money == 0) 0
    else {
      if (money < 0) 0
      else {
        if (coins.isEmpty) 0
        else (if (money == coins.head) 1 else countChange(money - coins.head, coins)) + countChange(money, coins.tail)
      }
    }
  }
}



