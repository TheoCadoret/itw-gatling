package io.gatling.interview.command

import io.gatling.interview.model.Computer
import org.scalatest.Inside
import org.scalatest.matchers.should
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class ComputerCommandTest extends AnyWordSpec with Inside with should.Matchers {

  "computer validation" should {
    val id = "3"
    val name = "MacBook Pro"
    val introducedString = "2016-01-26"
    val discontinuedString = "2016-01-27"
    "should return computer when params are valid" in {
      val computerEither =
        ComputerValidator.validateComputer(List(id, name, introducedString, discontinuedString))

      val computer = computerEither.getOrElse(fail("either was not Right!"))
      computer.id shouldBe 3
      computer.name shouldBe name
      val introduced = computer.introduced.getOrElse(fail("Introduced should be present"))
      introduced.isEqual(LocalDate.parse(introducedString))
      val discontinued = computer.discontinued.getOrElse(fail("Discontinued should be present"))
      discontinued.isEqual(LocalDate.parse(discontinuedString))
    }

    "should return computer without dates when not given " in {
      val computerEither = ComputerValidator.validateComputer(List(id, name))

      val computer = computerEither.getOrElse(fail("either was not Right!"))
      computer.id shouldBe 3
      computer.name shouldBe name
      computer.introduced shouldBe None
      computer.discontinued shouldBe None
    }

    "should fail when no name given" in {
      val computerEither = ComputerValidator.validateComputer(List(id))

      computerEither.isInvalid shouldBe true
    }

    "should fail when no id given" in {
      val computerEither = ComputerValidator.validateComputer(List())

      computerEither.isInvalid shouldBe true
    }

    "should fail when Id not Long" in {
      val computerEither =
        ComputerValidator.validateComputer(List("dvgz", name, introducedString, discontinuedString))

      computerEither.isInvalid shouldBe true
    }
    "should fail when introduced date not in proper format" in {
      val computerEither =
        ComputerValidator.validateComputer(List(id, name, "emogdoz", discontinuedString))

      computerEither.isInvalid shouldBe true
    }
    "should fail when discontinued date not in proper format" in {
      val computerEither =
        ComputerValidator.validateComputer(List(id, name, introducedString, "zrsvz"))

      computerEither.isInvalid shouldBe true
    }
  }
}
