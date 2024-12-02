package day2

import scala.io.Source
import scala.util.Using
import scala.util.boundary
import scala.util.boundary.break

@main def day2(): Unit =
  val m = readMatrix("input/input2.txt")
  val safeNum = m.count(x => isSafe(x))
  println(s"Safe: $safeNum") // 680

  val almostSafeNum = m.count(x => isSafe(x, 1))
  println(s"Almost safe: $almostSafeNum") // 710

def readMatrix(file: String): Seq[Seq[Int]] =
  Using(Source.fromFile(file)) { f =>
    f.getLines().map(line => {
      line.strip().split("\\s+").map(_.toInt).toSeq
    }).toSeq
  }.get

def isSafe(arr: Seq[Int], tolerance: Int = 0): Boolean =
  if (arr.size < 2) return true
  var prevDiff = 0
  boundary:
    for i <- Range(1, arr.size) do
      val diff = arr(i - 1) - arr(i)
      if (Math.abs(diff) <= 0 || Math.abs(diff) > 3
        || (prevDiff != 0 && prevDiff.sign != diff.sign)) {

        if (tolerance > 0) {
          break(
            Range(0, i + 1)
              .map(x => isSafe(except(arr, x), tolerance - 1))
              .reduce(_ || _)
          )
        } else {
          break(false)
        }
      }
      prevDiff = diff
    true

def except(arr: Seq[Int], i: Int): Seq[Int] =
  arr.slice(0, i) ++ (if i < arr.size - 1 then arr.slice(i + 1, arr.size) else Seq())



