def pascal(c: Int, r: Int): Int = {
  if (r == 0 || c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
}

pascal(0, 0)

pascal(0, 1)
pascal(1, 1)

pascal(0, 2)
pascal(1, 2)
pascal(2, 2)

pascal(0, 3)
pascal(1, 3)
pascal(2, 3)
pascal(3, 3)

pascal(0, 4)
pascal(1, 4)
pascal(2, 4)
pascal(3, 4)
pascal(4, 4)

def balance(chars: List[Char]) = {
  def balanceLoop(opened: Int, chars: List[Char]): Boolean = {
    if (chars.isEmpty) opened == 0 else balanceLoop(updateNoOfOpened(opened, chars.head), chars.tail)
  }

  def updateNoOfOpened(opened: Int, currentChar: Char) = {
    if (currentChar == '(') opened + 1 else if (opened > 0 && currentChar == ')') opened - 1 else opened
  }

  balanceLoop(0, chars)
}

balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)
balance("(if (zero? x) max (/ 1 x))".toList)
balance("())(".toList)
balance(")(()".toList)

def countChange(money: Int, coins: List[Int]): Int = {
  if (money == 0) 1 else if (money < 0 || coins.isEmpty) 0 else countChange(money, coins.tail) + countChange(money - coins.head, coins)
}

countChange(301, List(500, 5, 50, 100, 20, 200, 10))