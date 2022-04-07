package io.gatling.interview.command

import cats.data._
import cats.implicits._
import io.gatling.interview.model.Computer

object ComputerCommand {
  def parse(args: List[String]): Validated[Exception, ComputerCommand] = {
    val validatedCommand: ValidatedNel[Exception, ComputerCommand] = args match {
      case "list" :: _ => Some(ListComputers).toValidNel(new Exception("Never reached"))
      case "find" :: a :: _ =>
        a.toLongOption
          .map(it => FetchComputer(it))
          .toValidNel(new IllegalArgumentException("Id should be a Long"))
      case "insert" :: tail =>
        ComputerValidator.validateComputer(tail).map(it => InsertComputer(it))
      case _ => None.toValidNel(new IllegalArgumentException("Could not parse command"))
    }
    validatedCommand.leftMap[Exception](nel =>
      nel.reduceLeft[Exception]((a, b) => new Exception(s"${a.getMessage} \n ${b.getMessage}"))
    )
  }
}

sealed trait ComputerCommand

case object ListComputers extends ComputerCommand

case class FetchComputer(id: Long) extends ComputerCommand

case class InsertComputer(computer: Computer) extends ComputerCommand
