package advent_2023

import scala.annotation.tailrec

object Day09 extends App {

  def apply(inputFilePath: String): Unit = {
    println(s"Invoking ${this.getClass.getName} ...")
    val inputDataLine = readInputFile(inputFilePath).toList
    val result = process(inputDataLine)

    print(s"Result = $result")
  }

  def process(inputDataLine: List[String]) = {
    val formatInput: List[Array[Int]] = inputDataLine.map(_.split(" ").map(_.toInt))

//     for problem 9.1
    val resultForEachInput = formatInput.map(x => (x, findNextElement(x)))

//    for problem 9.2
    val resultForEachInput2 = formatInput.map(x => (x, findNextElement(x.reverse)))

    resultForEachInput.map(_._2).sum // OR resultForEachInput2.map(_._2).sum

  }

  def findNextElement(inputNumbers: Array[Int]): Int = {

    @tailrec
    def go(currentInput: Array[Int], nextNumber: Int): Int = {
      print(s"""
           | go -> ${currentInput.mkString("Array(", ", ", ")")}
           | ${currentInput.length} and ${nextNumber}
           |""".stripMargin)

      val newInputNumbers = for {
        i <- 0 until currentInput.length - 1
      } yield (currentInput(i + 1) - currentInput(i))

      val lastNumber: Int = currentInput(currentInput.length - 1)

      println(s"lastNumber = $lastNumber")

      if (newInputNumbers.count(_ == 0) == newInputNumbers.length) nextNumber + lastNumber
      else go(newInputNumbers.toArray, nextNumber + lastNumber)
    }
    val lineResult = go(inputNumbers, 0)

    println(s"Next Number for ${inputNumbers.mkString("Array(", ", ", ")")} is $lineResult")

    lineResult

  }

  private def readInputFile(inputFilePath: String): Iterator[String] =
    io.Source.fromResource(inputFilePath).getLines()

}
