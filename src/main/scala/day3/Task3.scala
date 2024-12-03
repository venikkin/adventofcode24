package day3

import java.io.{CharArrayReader, StringReader}
import java.util.Scanner
import scala.io.Source
import scala.util.Using

@main
def day3(): Unit =
  val s = Using(Source.fromFile("input/input3.txt")) { f =>
    f.mkString
  }.get

  val r = "mul\\((\\d{1,3}),(\\d{1,3})\\)".r
  var sum = 0
  for (m <- r.findAllIn(s).matchData) {
    sum += m.group(1).toInt * m.group(2).toInt
  }
  println(sum)





