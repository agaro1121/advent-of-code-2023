package adventofcode.day3

import scala.io.Source

// 114 and 58 not symbols
// expected: 4361
val sample =
"""|467..114..
   |...*......
   |..35..633.
   |......#...
   |617*......
   |.....+.58.
   |..592.....
   |......755.
   |...$.*....
   |.664.598..""".stripMargin

@main def solution() = {
  val fileContents = readFile("day3_input.txt")
  // val fileContents = sample.split("\n")
  val results = processLines(fileContents.toVector)
  val result = results.map(_.sum).sum
  println(s"result $result expected: 530849")
  result
}

@main def solutionPart2() = {
  val fileContents = readFile("day3_input.txt")
  // val fileContents = sample.split("\n")
  val results = part2(fileContents.toVector)
  val result = results.map{ _.sum }.sum
  println(s"result $result expected: 84900879")
  result
}

def processLines(lines: Vector[String]): Vector[List[Int]] = {
  val symbols: Set[Char] = Set('!', '@', '#', '$', '%', '^', '&', '*', '-', '+', '=', '/')
  val withIndex: Vector[(String, Int)] = lines.zipWithIndex
  
  val result = withIndex.map{
    case (line, lineIdx) =>
      val adjacentNumbers = collection.mutable.ListBuffer.empty[Int] // input has dupes. Can't be a `Set`
      val seenIndexes = collection.mutable.Set.empty[Int]
      val defaultChar = '?'
  
      line.zipWithIndex.foreach{ 
        case (cc, ccIdx) if cc.isDigit && !seenIndexes(ccIdx) =>

         val adjacentChars = for {
           dx <- -1 to 1
           dy <- -1 to 1
           newX = ccIdx + dx 
           newY = lineIdx + dy
         } yield {
           if( (0 < newX && newX < line.length - 1) && (0 < newY && newY < lines.length - 1) )
             lines(newY)(newX)
           else defaultChar 
         }

         val isAdjacent = adjacentChars.exists(symbols.contains)

         if (isAdjacent) {
           val wholeNum = getContinuousDigits(line, ccIdx)
           val usedIndexes = wholeNum.map(_._2).toList
           seenIndexes.addAll(usedIndexes)
           val wholeNumAsNumber = wholeNum.map(_._1).mkString.toInt
           adjacentNumbers.addOne(wholeNumAsNumber)
         }
        case _ => ()
      }
      adjacentNumbers.toList
  }
  result.filter(_.nonEmpty)
}

def part2(lines: Vector[String]): Vector[Vector[Int]] = {
  val withIndex: Vector[(String, Int)] = lines.zipWithIndex

  
  val result = withIndex.map{
    case (line, lineIdx) =>
      val ratios = collection.mutable.ListBuffer.empty[Int]

      line.zipWithIndex.toVector.foreach{
        case (cc, ccIdx) if cc == '*' =>
  
         // 1. Get adjacent Nums
         val adjacentNums = (for {
           dx <- -1 to 1
           dy <- -1 to 1
           newX = ccIdx + dx
           newY = lineIdx + dy
         } yield {
           if( (0 <= ccIdx && ccIdx <= line.length - 1) && (0 <= newY && newY <= lines.length - 1))
             getContinuousDigits(lines(newY), newX)
           else Vector()
         }).filter(_.nonEmpty)

         // 2. Get distinct - this solves the case of the digit right above/below the ccIdx being part of a number
         // 3 2 1 <- This produces 2 duplicates (3 total)
         //   *   <- we are here
         // 4 5 6 <- This produces 2 duplicates (3 total)
         val uniqueAdjacentNums = adjacentNums.distinct

         // 3. Count == 2
         val isAdjacent = uniqueAdjacentNums.size == 2 

         if (isAdjacent) {
           val uniqueNums = uniqueAdjacentNums.collect{ case num =>
               num.map(_._1).mkString.toInt
           }
           ratios.addOne(uniqueNums.product)
        }
      case _ => ()
    }
      ratios.toVector
  }
  result.filter(_.nonEmpty)
}

def getContinuousDigits(line: String, idx: Int): Vector[(Char, Int)] = {
  val c = line(idx)
  if(c.isDigit) {
    val withIndex = line.zipWithIndex.toVector
    val leftNums = withIndex.slice(0,idx).reverse.takeWhile(_._1.isDigit).reverse
    val rightNums = withIndex.slice(idx, line.length).takeWhile(_._1.isDigit)
    leftNums ++ rightNums
  } else Vector()
}

def readFile(file: String): Iterator[String] = Source.fromResource(file).getLines()
