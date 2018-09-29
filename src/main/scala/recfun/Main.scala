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
    */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 || c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceLoop(opened: Int, chars: List[Char]): Boolean = {
      if (opened < 0) false else if (chars.isEmpty) opened == 0 else balanceLoop(updateOpened(opened, chars.head), chars.tail)
    }

    def updateOpened(opened: Int, currentChar: Char) = {
      if (currentChar == '(') opened + 1 else if (currentChar == ')') opened - 1 else opened
    }

    balanceLoop(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1 else if (money < 0 || coins.isEmpty) 0 else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
