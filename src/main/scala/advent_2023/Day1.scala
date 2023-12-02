package advent_2023

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.matching.Regex

/** Day 1: Trebuchet?!
 * Link to problem : https://adventofcode.com/2023/day/1
  */

object Day1 {

  def apply(inputFilePath: String): Unit = {
    println(s"Invoking ${this.getClass.getName} ...")
    val inputDataLine = readInputFile(inputFilePath).toList

    val firstAndLastNumbersCombined = inputDataLine
      .map(fetchDigitsFromString)
      .map(digitsPerLine => {
        digitsPerLine(0) * 10 + digitsPerLine(digitsPerLine.length - 1)
      })

    val advancedProblem = inputDataLine
      .map(convertWordsToNumericForm) // for advanced problem
      .map(fetchDigitsFromString)
      .map(digitsPerLine => {
        digitsPerLine(0) * 10 + digitsPerLine(digitsPerLine.length - 1)
      })

    println(s"Result : ${firstAndLastNumbersCombined.sum} and ${advancedProblem.sum}")

  }

  private def convertWordsToNumericForm(input: String): String = {
    val wordRepresentationOfNumbersRegex = "one|two|three|four|five|six|seven|eight|nine"

    @tailrec
    def go(input: String): String = {
      val matchedWord: Option[Regex.Match] =
        wordRepresentationOfNumbersRegex.r.findFirstMatchIn(input)

      val replacingNumber = findReplacingNumberForMatchedText(matchedWord)

      replacingNumber match {
        case None => input
        case Some(numberAsString) =>
          go(input.replaceFirst(wordRepresentationOfNumbersRegex, numberAsString))
      }

    }
    go(input)

  }

  private def findReplacingNumberForMatchedText(
    matchedWord: Option[Regex.Match]
  ): Option[String] = {
    matchedWord match {
      case None => None
      case Some(value) =>
        value.group(0) match {
          case "one"   => Some("on1e")
          case "two"   => Some("tw2o")
          case "three" => Some("thre3e")
          case "four"  => Some("fou4r")
          case "five"  => Some("fiv5e")
          case "six"   => Some("si6x")
          case "seven" => Some("seve7n")
          case "eight" => Some("eigh8t")
          case "nine"  => Some("nin9e")
          case _       => None
        }
    }
  }

  private def readInputFile(inputFilePath: String): Iterator[String] =
    io.Source.fromResource(inputFilePath).getLines()

  private def fetchDigitsFromString(line: String): Array[Int] = {
    val numberRegex = raw"\d".r
    numberRegex.findAllMatchIn(line).map(_.toString.toInt).toArray
  }
}
