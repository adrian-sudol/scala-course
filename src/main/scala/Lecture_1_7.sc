def factorialInitial(value: Int) = {
  def factorialRec(res: Int, n: Int): Int = {
    if (n >= value) res * n else factorialRec(res * n, n + 1)
  }

  factorialRec(1, 1)
}

def factorialFixed(n: Int) = {
  def factorialRec(res: Int, n: Int): Int = {
    if (n == 0) res else factorialRec(res * n, n - 1)
  }

  factorialRec(1, n)
}

factorialInitial(0)

factorialInitial(4)

factorialInitial(5)

factorialFixed(0)

factorialFixed(4)

factorialFixed(5)