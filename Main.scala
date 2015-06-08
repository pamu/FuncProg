import scala.util.Try
import scala.util.Success

object Main {
  def main(args: Array[String]): Unit = {
    println(eval(""))
  }
  def eval(postfix: String): Try[Int] = {
    val list = postfix.foldLeft(List[Int]()) { (r: List[Int], c: Char) => {
      if (c.isDigit) {
        r ++ List(c.toString.toInt)
      } else if (! c.isSpaceChar) {
        if (c == '+') {
          r.take(r.size - 2) ++ List(r.takeRight(2).reduce(_ + _))
        } else if (c == '*') {
          r.take(r.size - 2) ++ List(r.takeRight(2).reduce(_ * _))
        } else if (c == '-') {
          r.take(r.size - 2) ++ List(r.takeRight(2).reduce(_ - _))
        } else {
          r.take(r.size - 2) ++ List(r.takeRight(2).reduce(_ / _))
        }
      } else {
        r
      }
    }}
    Success(list.head)
  }
}