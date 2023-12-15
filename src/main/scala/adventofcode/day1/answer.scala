package adventofcode.day1

import scala.io.Source
import scala.util.Using

// TODO: convert to scala-cli ?
@main def answer() = {
  val finalFile = "day1_input.txt"
  val fileContents = readFile(finalFile)
  val result = fileContents.map(findDigit).sum
  println(s"expected: 53855 actual: $result Correct? ${53855 == result}")
  result
}

def findDigit(line: String): Int = {
  val numberWords = Map("one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5, "six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9, "zero" -> 0)
  val keys = numberWords.keys

  def go(indexLeft: Int, indexRight: Int, resultLeft: Option[Int], resultRight: Option[Int]): Int = {
    if(resultLeft.isDefined && resultRight.isDefined)
      (resultLeft.get * 10) + resultRight.get
    else {
      val rLeft = resultLeft.orElse{
        val currentLeft = line(indexLeft)
        if(currentLeft.isDigit)
          Some(currentLeft.asDigit)
        else {
          keys.find{ numWord => 
            val currentWord = line.slice(indexLeft, indexLeft + numWord.length)
            numWord == currentWord
          }.flatMap(numberWords.get)
        }
      }

      val rRight = resultRight.orElse{
        val currentRight = line(indexRight)
        if(currentRight.isDigit)
          Some(currentRight.asDigit)
        else {
          keys.find{ numWord => 
            val currentWord = line.slice((indexRight + 1) - numWord.length, indexRight + 1)
            numWord == currentWord
          }.flatMap(numberWords.get)
        }
      }

      go(indexLeft + 1, indexRight - 1, rLeft, rRight)
    }
  }

  go(0, line.length - 1, None, None)
}

def readFile(file: String): Iterator[String] = Source.fromResource(file).getLines()
