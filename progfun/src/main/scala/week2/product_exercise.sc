object product_exercise {

  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a>b) 1 else f(a) * product(f)(a+1, b)

  def sum(f: Int => Int)(a: Int, b: Int): Int =
    if (a>b) 0 else f(a) + sum(f)(a+1, b)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+1, b))

  def product_mr(f: Int => Int)(a: Int, b: Int) =
    mapReduce(f, (x, y) => x*y, 1)(a, b)

  def factorial(a: Int): Int =
    if (a==0) 1 else a * factorial(a-1)

  def factorial1(n: Int) = product(x => x)(1, n)

  /** Tests */
  product(x => x)(1,3)
  product_mr(x => x)(1,3)

}