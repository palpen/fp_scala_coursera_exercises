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

    println(countChange(4, List(2, 1))) // 3
    println(countChange(4, List(10, 2, 1))) // 3
    println(countChange(4, List(4))) // 1
    println(countChange(4, List(4, 2))) // 2
    println(countChange(300, List(500, 5, 50, 100, 20, 200, 10))) // 1022
    println(countChange(301, List(5, 10, 20, 50, 100, 200, 500))) // 0
    println(countChange(5, List())) // 0
    println(countChange(0, List(1, 2, 3))) // 1

  }

  /**
    * Exercise 1
    * Sketch of solution:
    * val(c,r) = val(c-1, r-1) + val(c,r-1)
    * num columns = num rows + 1 (e.g. 4th row (index 3), has 4 columns)
    * Borders always have coordinates (0,row) or (row+1, row)
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    * Sketch of solution:
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
        if (chars.head == '(') go(chars.tail, rbc = rbc, lbc = lbc + 1)
        else if (chars.head == ')') go(chars.tail, rbc = rbc + 1, lbc = lbc)
        else go(chars.tail, rbc = rbc, lbc = lbc)
      }
    }

    go(chars, rbc = 0, lbc = 0)
  }

  /**
    * Exercise 3
    * Sketch of solution:
    *
    * Given integer N and a set of integer S, how many different ways
    * can we combine elements of S such that each given combination
    * sum up to N?
    *
    * Let S = {s(1),...,s(m)}
    * Let the number of ways be given by C(N,S).
    * A solution is a subset of S such that their sum == N.
    *
    * If a given solution contains s(m), then the problem we need
    * to solve reduces to solving, C(N-s(m), {s(1),...,s(m)}).
    * We are solving this problem to find the rest of the elements of
    * the solution containing s(m).
    * Note that in this case, we keep everything in
    * S because we can reuse every element across
    * solutions (e.g. 4 = 1+1+1+1 for C(4, {2,1}))
    *
    * Alternatively, if a given solution does NOT contain s(m)
    * then the problem we need to solve is the subproblem
    * that does not contain s(m): C(N, {s(1),...,s(m-1)})
    *
    * Give these two cases, finding the value of C(N, S)
    * amounts to finding the value of C(N-s(m), {s(1),...,s(m)})
    * and finding the value of C(N, {s(1),...,s(m-1)})
    * Therefore,
    * C(N,S) = C(N-s(m), {s(1),...,s(m)}) + C(N, {s(1),...,s(m-1)})
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0) 0
    else if (money == 0) 1 // exchange 0 for 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
