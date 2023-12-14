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
  val results = processLines(fileContents.toArray)
  val result = results.map(_.sum).sum
  println(s"result $result expected: 530849")
}

@main def solutionPart2() = {
  val fileContents = readFile("day3_input.txt")
  // val fileContents = sample.split("\n")
  val results = part2(fileContents.toArray)
  val result = results.map{ _.sum }.sum
  println(s"result $result expected: 84900879")
}

def processLines(lines: Array[String]): Array[List[Int]] = {
  val symbols: Set[Char] = Set('!', '@', '#', '$', '%', '^', '&', '*', '-', '+', '=', '/')
  val withIndex: Array[(String, Int)] = lines.zipWithIndex
  
  val result = withIndex.map{
    case (line, lineIdx) =>
      val adjacentNumbers = collection.mutable.ListBuffer.empty[Int] // input has dupes. Can't be a `Set`
      val seenIndexes = collection.mutable.Set.empty[Int]
      val defaultChar = '?'
  
      line.zipWithIndex.foreach{ 
        case (cc, ccIdx) if cc.isDigit && !seenIndexes(ccIdx) =>
         val leftChar = if(ccIdx > 0) line(ccIdx - 1) else defaultChar
         val rightChar = if(ccIdx <= line.length - 2) line(ccIdx + 1) else defaultChar
         val upChar = if(lineIdx > 0) lines(lineIdx - 1)(ccIdx) else defaultChar
         val downChar = if(lineIdx <= lines.length - 2) lines(lineIdx + 1)(ccIdx) else defaultChar

         val upLeftChar = if(ccIdx > 0 && lineIdx > 0) lines(lineIdx - 1)(ccIdx - 1) else defaultChar
         val upRightChar = if(lineIdx > 0 && ccIdx <= line.length - 2) lines(lineIdx - 1)(ccIdx + 1) else defaultChar
         val downLeftChar = if(ccIdx > 0 && lineIdx < lines.length - 2) lines(lineIdx + 1)(ccIdx - 1) else defaultChar
         val downRightChar = if(lineIdx <= lines.length - 2 && ccIdx <= line.length - 2) lines(lineIdx + 1)(ccIdx + 1) else defaultChar

         val adjacentChars = Array(leftChar, rightChar, upChar, downChar, upLeftChar, upRightChar, downLeftChar, downRightChar)
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

def part2(lines: Array[String]): Array[List[Int]] = {
  val withIndex: Array[(String, Int)] = lines.zipWithIndex
  
  val result = withIndex.map{
    case (line, lineIdx) =>
      val ratios = collection.mutable.ListBuffer.empty[Int]

      line.zipWithIndex.foreach{ 
        case (cc, ccIdx) if cc == '*' =>
         val leftNum = if(ccIdx > 0) {
            getContinuousDigits(line, ccIdx - 1)
           } else IndexedSeq()

         val rightNum = if(ccIdx <= line.length - 2){
           getContinuousDigits(line, ccIdx + 1)
         } else IndexedSeq()

         val upNum = if(lineIdx > 0) {
            getContinuousDigits(lines(lineIdx - 1), ccIdx)
         }  else IndexedSeq()

         val downNum = if(lineIdx <= lines.length - 2){
          getContinuousDigits(lines(lineIdx + 1), ccIdx)
         } else IndexedSeq()

         val upLeftNum = if(ccIdx > 0 && lineIdx > 0){
             getContinuousDigits(lines(lineIdx - 1), ccIdx - 1)
          } else IndexedSeq()

         val upRightNum = if(lineIdx > 0 && ccIdx <= line.length - 2){
           getContinuousDigits(lines(lineIdx - 1), ccIdx + 1)
         } else scala.collection.IndexedSeq()

         val downLeftNum = if(ccIdx > 0 && lineIdx <= lines.length - 2) {
            getContinuousDigits(lines(lineIdx + 1), ccIdx - 1)
         } else IndexedSeq()

         val downRightNum = if(lineIdx <= lines.length - 2 && ccIdx <= line.length - 2){
           getContinuousDigits(lines(lineIdx + 1), ccIdx + 1)
         } else IndexedSeq()
  
         // 1. Get adjacent Nums
         val adjacentNums = Array(leftNum, rightNum, upNum, downNum, upLeftNum, upRightNum, downLeftNum, downRightNum).filter(_.nonEmpty)

         // 2. Get distinct
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
      ratios.toList

  }
    result.filter(_.nonEmpty)
}

def getContinuousDigits(line: String, idx: Int): IndexedSeq[(Char, Int)] = {
  val c = line(idx)
  if(c.isDigit) {
    val withIndex = line.zipWithIndex
    val leftNums = withIndex.slice(0,idx).reverse.takeWhile(_._1.isDigit).reverse
    val rightNums = withIndex.slice(idx, line.length).takeWhile(_._1.isDigit)
    leftNums ++ rightNums
  } else IndexedSeq()
}

def readFile(file: String): Iterator[String] = Source.fromResource(file).getLines()

