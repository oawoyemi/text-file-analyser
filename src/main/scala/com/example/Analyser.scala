package com.example

import java.io.File

import scala.io.Source

object AnalyserApp extends App {
  if (args.length != 1) println("usage: <filename>")

 println(Analyser.calculate(new File(args(0)), Statistics.wordCount))
}

object Analyser {

  def calculate[T, V](f: File, s: Statistic[T, V]): Result[V] = s.reduce(Source.fromFile(f).getLines().map(s.map))

}

case object Result {
  val zero = Result(0.0)
}

case class Result[T](value: T)

case class Statistic[T, V](map: String => T, reduce: Iterator[T] => Result[V])

object Statistics {

  val wordCount = Statistic(
    map = (_: String).split(" ").length,
    reduce = (i: Iterator[Int]) => Result(i.sum)
  )
  val lineCount = Statistic(
    map = (_: String) => 1,
    reduce = (i: Iterator[Int]) => Result(i.sum)
  )
  val averageLetterPerWord = Statistic(
    map = (line: String) => charArrayWithoutWhitespace(line).length.toDouble / line.split(" ").length.toDouble,
    reduce = (i: Iterator[Double]) => {
      val (it1, it2) = i.duplicate
      val sum = it1.sum
      if (sum == 0) Result.zero else Result(rounding(sum / it2.size.toDouble))
    }
  )

  val mostCommonLetter = Statistic(
    map = (line: String) => charArrayWithoutWhitespace(line).groupBy(char => char).map { case (c, cs) => c -> cs.length },
    reduce = (lineCharCounts: Iterator[Map[Char, Int]]) => Result(
      lineCharCounts.foldLeft(Map.empty[Char, Int])(aggregateValues) match {
        case charCounts if charCounts.nonEmpty =>
          val (char, _) = findMax(charCounts)
          Some(char)
        case _                                 => None
      }
    )

  )

  def findMax(charCounts: Map[Char, Int]): (Char, Int) =
    charCounts.maxBy { case (_, count) => count }

  def rounding(d: Double) = f"$d%.1f".toDouble

  def charArrayWithoutWhitespace(s: String) = s.toCharArray.filterNot(_ == ' ')

  def aggregateValues[T] = (map1: Map[T, Int], map2: Map[T, Int]) => map1 ++ map2.map {
    case (k, v) => k -> (v + map1.getOrElse(k, 0))
  }
}
