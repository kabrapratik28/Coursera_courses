import scala.annotation.tailrec

def abs(x: Double) = if (x < 0) -x else x

def sqrt(x: Double) = {
  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  def isGoodEnough(guess: Double, x: Double): Boolean =
    abs(guess * guess - x) / x < 0.0001

  def improve(guess: Double, x: Double) = (guess + x / guess) / 2

  sqrtIter(1.0, x)
}

sqrt(1e50)

//tail recursion factorial
def takeFactorial(factorialNumber: Int): Int = {

  @tailrec
  def factorial(x: Int,output: Int): Int ={
    if (x+1>factorialNumber) output
    else factorial(x+1,output * (x+1))
  }

  factorial(0,1)
}

takeFactorial(5)