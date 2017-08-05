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
      if(c==0 && r==0) {1}
      else{
        var leftUp = c-1 >= 0
        var rightUp = c <= r-1
        var left = 0
        var right = 0
        if(leftUp){
          left = pascal(c-1,r-1)
        }
        if(rightUp){
          right = pascal(c,r-1)
        }
        left + right
      }
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def _balance(chars: List[Char], openRemained : Int): Boolean ={
        if (chars.isEmpty){
          if (openRemained==0) true else false
        }else if(openRemained<0){
          false
        }else{
          var curr = chars.head
          if(curr=='('){
            _balance(chars.tail,openRemained+1)
          }else if(curr==')'){
            _balance(chars.tail,openRemained-1)
          }else{
            _balance(chars.tail,openRemained)
          }
        }
      }
      _balance(chars,0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def _count(money: Int, index: Int): Int ={
        if (money==0) return 1
        if (index<0 || money<0) return 0
        _count(money,index-1) + _count(money-coins(index),index)
      }

      _count(money,coins.length-1)
    }
  }
