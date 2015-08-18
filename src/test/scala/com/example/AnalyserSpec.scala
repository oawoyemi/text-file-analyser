package com.example

import java.io.{FileWriter, File}

import org.scalatest.{BeforeAndAfter, WordSpec, Matchers}
import com.example.Statistics._

class AnalyserSpec extends WordSpec with Matchers with BeforeAndAfter {
  val testFile = new File("test.txt")
  val fileWith = (s: String) => {val fw = new FileWriter(testFile); fw.write(s); fw.close(); testFile}

  "The text file analyser" when {

    "computing a whitespace delimited word count for a text file" should {
      "calculate a word count of 0 processing an empty file" in {
        Analyser.calculate(fileWith(""), wordCount) shouldBe Result(0)
      }
      "calculate a word count of 1 for a file containing 'hello' " in {
        Analyser.calculate(fileWith("hello"), wordCount) shouldBe Result(1)
      }
      "calculate a word count of 2 for a file containing 'hello world' " in {
        Analyser.calculate(fileWith("hello world"), wordCount) shouldBe Result(2)
      }
      "calculate a word count of 4 for a file containing 'hello world \nfoo bar' " in {
        Analyser.calculate(fileWith("hello world\nfoo bar"), wordCount) shouldBe Result(4)
      }

    }
    "computing a line count for a text file" should {
      "calculate a line count of 0 for an empty file" in {
        Analyser.calculate(fileWith(""), lineCount) shouldBe Result(0)
      }
      "calculate a line count of 1 for a file containing 'line1'" in {
        Analyser.calculate(fileWith("line1"), lineCount) shouldBe Result(1)
      }
      "calculate a line count of 2 for a file containing 'line1\nline2'" in {
        Analyser.calculate(fileWith("line1\nline2"), lineCount) shouldBe Result(2)
      }
    }

    "computing the average number of letters per word to one decimal place in a file" should {
      "calculate an average of 0 for an empty file" in {
        Analyser.calculate(fileWith(""), averageLetterPerWord) shouldBe Result(0.0)
      }
      "calculate an average of 3 for a file containing 'foo'" in {
        Analyser.calculate(fileWith("foo"), averageLetterPerWord) shouldBe Result(3.0)
      }
      "calculate an average of 4.5 for a file containing 'foo barbar'" in {
        Analyser.calculate(fileWith("foo barbar"), averageLetterPerWord) shouldBe Result(4.5)
      }
      "calculate an average of 4.8 for a file containing 'foo barbar'" in {
        Analyser.calculate(fileWith("foo barbar\nlorum ipsum"), averageLetterPerWord) shouldBe Result(4.8)
      }
    }

    "computing the most common letter" should {
      "return None for an empty file" in {
        Analyser.calculate(fileWith(""), mostCommonLetter) shouldBe Result(None)
      }
      "calculate the letter as 'b' for a file containing 'b'" in {
        Analyser.calculate(fileWith("b"), mostCommonLetter) shouldBe Result(Some('b'))
      }

      "calculate the letter as 'a' for a file containing 'a foo bar bar bar'" in {
        Analyser.calculate(fileWith("a foo bar bar bar"), mostCommonLetter) shouldBe Result(Some('a'))
      }

      "calculate the letter as 'o' for a file containing 'a foo bar bar bar\nfoo foo foo'" in {
        Analyser.calculate(fileWith("a foo bar bar bar\nfoo foo foo"), mostCommonLetter) shouldBe Result(Some('o'))
      }
    }

    "a new statistical computations" should {
      "execute and return a statistical result" in {
        Analyser.calculate(
          fileWith("tango uniform tango"),
          Statistic(map = (_: String).count(_ == 't'), reduce = (count: Iterator[Int]) => Result(count.sum))
        ) shouldBe Result(2)
      }
    }
  }

  after {testFile.delete()}
}
