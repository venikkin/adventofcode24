package day3

import java.io.{CharArrayReader, StringReader}
import java.util.Scanner
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using
import scala.util.boundary

@main
def day3(): Unit =
  val s = Using(Source.fromFile("input/input3.txt")) { f =>
    f.mkString
  }.get

  val nums1 = solveWithRegex(s)
  println(nums1)
  println(nums1.sum)

  val num2 = solveWithIter(s)
  println(num2)
  println(num2.sum)
  println(solveWithIter(s, true).sum)

def solveWithRegex(s: String): List[Int] =
  val r = "mul\\((\\d{1,3}),(\\d{1,3})\\)".r
  val sum = ListBuffer[Int]()
  for (m <- r.findAllIn(s).matchData) {
    sum.addOne(m.group(1).toInt * m.group(2).toInt)
  }
  sum.toList
  
def solveWithIter(s: String, dos: Boolean = false): List[Int] =
  val sum2 = ListBuffer[Int]()
  var i = 0
  var include = true
  while (i < s.length) {
    if (i + 4 < s.length && s.substring(i, i + 4) == "mul(" && include) {
      var j = i + 4
      var num1 = 0
      var num2 = 0
      boundary:
        while (j < s.length) {
          val c = s(j)
          if (c.isDigit) {
            num1 = num1 * 10 + c.toString.toInt
          } else if (c == ',') {
            num2 = num1
            num1 = 0
          } else if (c == ')') {
            sum2.addOne(num1 * num2)
            boundary.break()
          } else {
            boundary.break()
          }
          j += 1
        }
      i += 1
    } else if (i + 4 < s.length && s.substring(i, i + 4) == "do()") {
      include = true
      i += 4
    } else if (i + 7 < s.length && s.substring(i, i + 7) == "don't()" && dos) {
      include = false
      i += 7
    } else {
      i += 1
    }

  }
  sum2.toList



