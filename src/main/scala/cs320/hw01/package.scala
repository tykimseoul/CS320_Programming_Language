package cs320

package object hw01 extends Homework01 {
  // Problem 1
  def dollar2won(dollar: Int): Int = dollar * 1100

  def volumeOfCuboid(a: Int, b: Int, c: Int): Int = a * b * c

  def isEven(num: Int): Boolean = num % 2 == 0

  def isOdd(num: Int): Boolean = num % 2 != 0

  //assuming positive inputs
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  //assuming positive inputs
  def lcm(a: Int, b: Int): Int = (a * b) / gcd(a, b)

  // Problem 2
  def numOfHomework(course: COURSE): Int = course match {
    case CS320(_, homework) => homework
    case CS311(homework) => homework
    case CS330(_, homework) => homework
  }

  def hasProjects(course: COURSE): Boolean = course match {
    case CS320(_, _) => false
    case CS311(_) => false
    case CS330(projects, _) => projects > 0
  }

  // Problem 3
  def namePets(pets: List[String]): List[String] = pets.map {
    case "dog" => "happy"
    case "cat" => "smart"
    case "pig" => "pinky"
    case other => other
  }

  def giveName(oldName: String, newName: String): List[String] => List[String] = {
    oldList: List[String] =>
      oldList.map {
        case `oldName` => newName
        case other => other
      }
  }

  def tests: Unit = {
    test(dollar2won(1), 1100)
    test(volumeOfCuboid(1, 2, 3), 6)
    test(isEven(10), true)
    test(isOdd(10), false)
    test(gcd(123, 245), 1)
    test(lcm(123, 245), 30135)
    test(numOfHomework(CS320(quiz = 4, homework = 3)), 3)
    test(hasProjects(CS320(quiz = 3, homework = 9)), false)
    test(namePets(List("dog", "cat", "pig")), List("happy", "smart", "pinky"))
    test(giveName("bear", "pooh")(List("pig", "cat", "bear")), List("pig", "cat", "pooh"))

    /* Write your own tests */
    test(dollar2won(0), 0)
    test(dollar2won(-1), -1100)
    test(volumeOfCuboid(-1, 1, 1), -1)
    test(volumeOfCuboid(0, 1, 1), 0)
    test(volumeOfCuboid(1, 1, 1), 1)
    test(isEven(0), true)
    test(isEven(1), false)
    test(isEven(-1), false)
    test(isOdd(0), false)
    test(isOdd(1), true)
    test(isOdd(-1), true)
    test(gcd(10, 22), 2)
    test(gcd(235, 457), 1)
    test(gcd(1, 235), 1)
    test(lcm(54, 23), 1242)
    test(lcm(1, 2), 2)
    test(lcm(1345, 235), 63215)
    test(numOfHomework(CS320(quiz = 5, homework = 2)), 2)
    test(numOfHomework(CS320(homework = 5, quiz = 2)), 5)
    test(numOfHomework(CS311(homework = 3)), 3)
    test(numOfHomework(CS330(homework = 7, projects = 2)), 7)
    test(hasProjects(CS320(quiz = 5, homework = 2)), false)
    test(hasProjects(CS311(homework = 3)), false)
    test(hasProjects(CS330(homework = 7, projects = 2)), true)
    test(hasProjects(CS330(homework = 7, projects = 0)), false)
    test(namePets(List("lion")), List("lion"))
    test(namePets(List("dog", "cat", "pig", "lion")), List("happy", "smart", "pinky", "lion"))
    test(namePets(List("dog", "cat", "pig", "lion", "lion")), List("happy", "smart", "pinky", "lion", "lion"))
    test(giveName("lion", "long")(List("lion")), List("long"))
    test(giveName("lion", "long")(List("dog", "cat", "pig", "lion")), List("dog", "cat", "pig", "long"))
    test(giveName("lion", "long")(List("dog", "cat", "pig", "lion", "lion")), List("dog", "cat", "pig", "long", "long"))
  }
}