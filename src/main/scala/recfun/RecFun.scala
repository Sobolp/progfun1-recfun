package recfun

object RecFun extends RecFunInterface :

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (r == 0 || c <= 0 || c >= r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    def countList(count: Int, list: List[Char]): (Int, List[Char]) =
      if (list.isEmpty || count < 0)
        (count, list)
      else if (list.head == '(')
        countList(count + 1, list.tail)
      else if (list.head == ')')
        countList(count - 1, list.tail)
      else countList(count, list.tail)

    countList(0, chars)._1 == 0
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)




