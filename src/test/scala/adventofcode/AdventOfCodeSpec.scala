package adventofcode

class AdventOfCodeSpec extends munit.FunSuite {
  test("day1_input") {
    val expected = 53855
    val actual = day1.answer()
    assertEquals(expected, actual)
  }

  test("day2_input_part1") {
    val expected = 2169
    val actual = day2.part1()
    assertEquals(expected, actual)
  }
  test("day2_input_part2") {
    val expected = 60948
    val actual = day2.part2()
    assertEquals(expected, actual)
  }

  test("day3_input_part1") {
    val expected = 530849
    val actual = day3.solution()
    assertEquals(expected, actual)
  }
  test("day3_input_part2") {
    val expected = 84900879
    val actual = day3.solutionPart2()
    assertEquals(expected, actual)
  }


  // test("day4_sample_part1") {
  //   val expected = -1
  //   val actual = day4.sample_part1()
  //   assertEquals(expected, actual)
  // }
  // test("day4_sample_part2") {
  //   val expected = -1
  //   val actual = day4.sample_part2()
  //   assertEquals(expected, actual)
  // }
  // test("day4_input_part1") {
  //   val expected = -1
  //   val actual = day4.input_part1()
  //   assertEquals(expected, actual)
  // }
  // test("day4_input_part2") {
  //   val expected = -1
  //   val actual = day4.input_part2()
  //   assertEquals(expected, actual)
  // }

}
