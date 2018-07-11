object higher_order_functions {
  /** Naive way to do sum */
  def sumInts0(a: Int, b:Int): Int =
    if (a>b) 0 else a + sumInts0(a+1, b)

  def cube(x: Int): Int = x*x*x
  def sumCubes0(a: Int, b: Int): Int =
    if (a>b) 0 else cube(a) + sumCubes0(a+1, b)

  def fact(x: Int): Int = x*fact(x-1)
  def sumFactorials0(a: Int, b: Int): Int =
    if (a>b) 0 else fact(a) + sumFactorials0(a+1, b)

  /** Better way to do sum */
  def sum1(f: Int => Int, a: Int, b: Int): Int =
    if(a>b) 0 else f(a) + sum1(f, a+1, b)
  def sumInts1(a: Int, b: Int) = sum1(Int => Int, a, b)
  def sumCubes1(a: Int, b: Int)= sum1(cube, a, b)
  def sumFactorials1(a: Int, b: Int) = sum1(fact, a, b)

  /** Using Anonymous functions */
  (x: Int) => x*x*x
  def sum2(f: Int => Int, a: Int, b: Int): Int =
    if(a>b) 0 else f(a) + sum2(f, a+1, b)
  def sumInts2(a: Int, b: Int) = sum2(Int => Int, a, b)
  def sumCubes2(a: Int, b: Int)= sum2(x => x*x*x, a, b)
  def sumFactorials2(a: Int, b: Int) = sum2(fact, a, b)

  /** Using functions that return functions (sum function wrapper) */
  /** The general form of this function is
    * def f(args_1)...(args_n-1) = {def g(args_n) = E; g}
    * Concisely: def f(args_1)...(args_n-1) = (args_n => E)
    * */
  def sum3(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a>b) 0
      else f(a) + sumF(a+1, b)
    sumF
  }

  def sumInts3 = sum3(x=>x)
  def sumCubes3 = sum3(x => x*x*x)
  def sumFactorials3 = sum3(fact)

  sumInts3(1,4)
  sum3(Int => Int)(1,4)
  sum3(cube)(1,4)

  /** Shorter way to write sum function wrapper */
  def sum4(f: Int => Int)(a: Int, b: Int): Int =
    if (a>b) 0 else f(a) + sum4(f)(a+1, b)  // note recursive application of sum4
//  sum4(Int => Int)
//  sum4(Int => Int)(1,2)
}