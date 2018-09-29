def abs(x: Double) = if (x < 0) -x else x

def sqrt(x: Double) = {
  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def isGoodEnough(guess: Double) = abs(guess * guess - x) < 0.01 * x

  def improve(guess: Double) = (guess + x / guess) / 2

  sqrtIter(1.0)
}

val testLo = sqrt(1e-60)

val testHi = sqrt(1e60)