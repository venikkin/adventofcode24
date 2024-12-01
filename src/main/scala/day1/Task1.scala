package day1

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Using

@main def solve(): Unit =
  val (a, b) = readAndParse("input/input1.txt")
  val sortedA = indexAndSort(a)
  val sortedB = indexAndSort(b)
  
  val sumOfDistance = sortedA.zip(sortedB).map(x => {
    Math.abs(x._1 - x._2)
  }).sum()
  println(s"Distance: $sumOfDistance")
  
  val counts = b.groupBy(x => x).view.mapValues(_.size)
  val similarities = sortedA.map(x => x * counts.getOrElse(x, 0)).sum
  println(s"Simil: $similarities")

def readAndParse(f: String): (List[Int], List[Int]) =
  val a = ListBuffer[Int]()
  val b = ListBuffer[Int]()

  val lines = Using(Source.fromFile(f)) { s =>
    for (line <- s.getLines()) {
      val nums = line.strip().split("\\s+").map(_.toInt)
      a.addOne(nums(0))
      b.addOne(nums(1))
    }
  }

  (a.toList, b.toList)

def indexAndSort(l: List[Int]): List[Int] =
  l.zipWithIndex.sortBy({ x => (x._1, x._2)}).map(_._1)