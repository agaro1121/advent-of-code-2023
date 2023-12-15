package adventofcode.day2

import scala.io.Source

case class GameSet(red: Int, green: Int, blue: Int)
case class Game(id: Int, sets: List[GameSet] )

@main def part1() = {
  val fileContents = readFile("day2_input.txt").toList
  val redLimit: Int = 12
  val greenLimit: Int = 13
  val blueLimit: Int = 14
  val isValidGameSet: GameSet => Boolean = gameSet =>
    gameSet.red <= redLimit && gameSet.green <= greenLimit && gameSet.blue <= blueLimit
  val isValidGame: Game => Boolean = game =>
    game.sets.forall(isValidGameSet)
  // val testStr: String = "Game 1: 2 red, 2 green; 6 red, 3 green; 2 red, 1 green, 2 blue; 1 red"
  // val testGame = processLine(testStr)
  // val testMaxes = 
  //   (testGame.sets.maxBy(_.red).red,
  //   testGame.sets.maxBy(_.green).green,
  //   testGame.sets.maxBy(_.blue).blue)
  // println(s"testGame $testGame testMaxes $testMaxes testPower ${testMaxes._1 * testMaxes._2 * testMaxes._3}")
  val validGames = fileContents.map(processLine).filter(isValidGame)
  val sumOfIds = validGames.map(_.id).sum
  println(s"sum $sumOfIds expected: 2169")
  sumOfIds
}

@main def part2() = {
  // Part 2
  val fileContents = readFile("day2_input.txt").toList

  val maxes = fileContents.map(processLine).map{ game =>
    (game.sets.maxBy(_.red).red,
    game.sets.maxBy(_.green).green,
    game.sets.maxBy(_.blue).blue)
  }
  val powers = 
    maxes.map{case (r, g, b) => r * g * b}

  val powersSum = powers.sum
  println(s"powerSum $powersSum expected: 60948")
  powersSum
}
@main def solution() = {
  val fileContents = readFile("day2_input.txt").toList

  // Part 1
  val redLimit: Int = 12
  val greenLimit: Int = 13
  val blueLimit: Int = 14
  val isValidGameSet: GameSet => Boolean = gameSet =>
    gameSet.red <= redLimit && gameSet.green <= greenLimit && gameSet.blue <= blueLimit
  val isValidGame: Game => Boolean = game =>
    game.sets.forall(isValidGameSet)

  // val testStr: String = "Game 1: 2 red, 2 green; 6 red, 3 green; 2 red, 1 green, 2 blue; 1 red"
  // val testGame = processLine(testStr)
  // val testMaxes = 
  //   (testGame.sets.maxBy(_.red).red,
  //   testGame.sets.maxBy(_.green).green,
  //   testGame.sets.maxBy(_.blue).blue)
  // println(s"testGame $testGame testMaxes $testMaxes testPower ${testMaxes._1 * testMaxes._2 * testMaxes._3}")
  val validGames = fileContents.map(processLine).filter(isValidGame)
  val sumOfIds = validGames.map(_.id).sum
  println(s"sum $sumOfIds expected: 2169")

  // Part 2
  val maxes = fileContents.map(processLine).map{ game =>
    (game.sets.maxBy(_.red).red,
    game.sets.maxBy(_.green).green,
    game.sets.maxBy(_.blue).blue)
  }
  val powers = 
    maxes.map{case (r, g, b) => r * g * b}

  val powersSum = powers.sum
  println(s"powerSum $powersSum expected: 60948")
}

// sample input
// Game 1:
// 2 red, 2 green;
// 6 red, 3 green;
// 2 red, 1 green, 2 blue;
// 1 red
def processLine(line: String): Game = {
  val everything = line.split(":")
  val gameId = everything.head.split(" ")(1).toInt

  val gameSets = everything.tail
    .flatMap(_.split(";"))
    .map(_.trim())
    .map(_.split(",").map(_.trim()))

  def processSet(lines: Array[String]): GameSet = {
    lines
      .map(_.split(" "))
      .foldLeft(GameSet(0, 0, 0)){
        (acc, curr) =>
          val n = curr(0).toInt
          val color = curr(1).toLowerCase()
          if("red" == color)
            acc.copy(red = n)
          else if("green" == color)
            acc.copy(green = n)
          else acc.copy(blue = n)
    }
  }
  val sets = gameSets.map(processSet).toList
  Game(gameId, sets)
}

def readFile(file: String): Iterator[String] = Source.fromResource(file).getLines()

