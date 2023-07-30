package recfun

import scala.collection.mutable

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do print(s"${pascal(col, row)} ")
      println()

  /** Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) {
      1
    } else {
      pascal(c - 1, r) + pascal(c, r - 1)
    }

  /** Exercise 2
    */
  def balance(chars: List[Char]): Boolean =
    var stack: mutable.Stack[Char] = mutable.Stack.empty
    for (c <- chars) {
      if (c == '(') {
        stack = stack.push(c)
      } else if (c == ')') {
        if (stack.nonEmpty && stack.last == '(') {
          stack.pop()
        } else {
          stack.push(c)
        }
      }
    }

    stack.isEmpty

  /** Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) {
      1
    } else if (money < 0 || coins.isEmpty) {
      0
    } else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
