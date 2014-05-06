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
    if (c == 0 || c == r) return 1
    else return pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balance(chars: List[Char], beginNumber: Int): Boolean = {

      if (beginNumber == 0 && chars.isEmpty) return true
      if (beginNumber < 0) return false
      if (beginNumber > 0 && chars.isEmpty) return false
      else {
        if (chars.head == '(') return balance(chars.tail, beginNumber + 1)
        if (chars.head == ')') return balance(chars.tail, beginNumber - 1)
        else return balance(chars.tail, beginNumber)
      }
    }

    return balance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    if (money == 0) return 0
    if (money < 0) return 0
    if (coins.isEmpty) return 0
    else return (if (money == coins.head) 1 else countChange(money - coins.head, coins)) + countChange(money, coins.tail)
  }
}



