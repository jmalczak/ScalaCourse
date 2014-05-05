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
    if(c == 0 || c == r) return 1   
    else return pascal(c -1, r -1) + pascal(c, r -1)    
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
     
    def balance(chars: List[Char], beginNumber: Int): Boolean = {
      
      if(beginNumber == 0 && chars.isEmpty) return true
      if(beginNumber < 0) return false
      if(beginNumber > 0 && chars.isEmpty) return false
      else {
        if(chars.head == '(') return balance(chars.tail, beginNumber + 1)
        if(chars.head == ')') return balance(chars.tail, beginNumber - 1)
        else return balance(chars.tail, beginNumber)
      }            
    }
       
    return balance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
    print(coins.toString + " money " + money +  "\n")
    
    if(money == 0) return 0
    if(coins.isEmpty) return 0
    else{
         
      def separateCountChanges =  for(i <- coins) yield {	
        
        if(money == i) 1
        else {
          if(money < i) 0
          else countChange(money - i, coins)
        }
      }
            
      print(separateCountChanges.toString + "\n")
  
      
      return if(separateCountChanges.forall(p => p == 1)) 1 else 0 
    }
  }
}
