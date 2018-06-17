package recfun

object Main {
  def main(args: Array[String]) {
//    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row)
//        print(pascal(col, row) + " ")
//      println()
//    }

//    println(balance("(if (zero? x) max (/ 1 x))".toList)) // true
//    println(balance("".toList))  // true
//    println(balance("())".toList))  // false
//    println(balance("((())".toList))  // false
//    println(balance("(if (zero? x) max (/ 1 x)))".toList)) // false
//    println(balance("(if (zero? x) max (/ 1 x)()".toList)) // false
//    println(balance("())(".toList))  // false

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
    * If at the end num. of left bracket != num. right bracket
    * and list is empty, false
    * If number of left bracket < right bracket, false immediately
    * if list is empty from start, true
   */
    def balance(chars: List[Char]): Boolean = {
      @annotation.tailrec
      def go(chars: List[Char], rbc: Int, lbc: Int): Boolean = {
        if (chars.isEmpty && lbc != rbc) false
        else if (chars.isEmpty) true
        else if (lbc < rbc) false
        else {
          if (chars.head == '(') go(chars.tail, rbc=rbc, lbc=lbc+1)
          else if (chars.head == ')') go(chars.tail, rbc=rbc+1, lbc=lbc)
          else go(chars.tail, rbc=rbc, lbc=lbc)
        }
      }

      go(chars, rbc=0, lbc=0)

    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
