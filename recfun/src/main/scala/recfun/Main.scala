package recfun

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
    * val(c,r) = val(c-1, r-1) + val(c,r-1)
    * num columns = num rows + 1 (e.g. 4th row (index 3), has 4 columns)
    * Borders always have coordinates (0,row) or (row+1, row)
   */

    def pascal(c: Int, r: Int): Int = {
      if (c==0 || c==r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = ???
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
