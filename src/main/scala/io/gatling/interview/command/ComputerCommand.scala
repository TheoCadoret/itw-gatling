package io.gatling.interview.command

import cats.data._
import cats.implicits._
import io.gatling.interview.model.Computer

import java.time.LocalDate
import scala.util.Try
import scala.util.control.NonFatal

object ComputerCommand {
  def parse(args: List[String]): Validated[Exception, ComputerCommand] = {
    val validatedCommand: ValidatedNel[Exception, ComputerCommand] = args match {
      case "list" :: _ => Some(ListComputers).toValidNel(new Exception("Never reached"))
      case "find" :: a :: _ =>
        a.toLongOption
          .map(it => FetchComputer(it))
          .toValidNel(new IllegalArgumentException("Id should be a Long"))
      case "insert" :: tail =>
        ComputorValidator.validateComputer(tail).map(it => InsertComputer(it))
      case _ => None.toValidNel(new IllegalArgumentException("Could not parse command"))
    }
    validatedCommand.leftMap[Exception](nel =>
      nel.reduceLeft[Exception]((a, b) => new Exception(s"${a.getMessage} \n ${b.getMessage}"))
    )
  }
}

object ComputorValidator {
  def validateComputer(args: List[String]): ValidatedNel[Exception, Computer] = {
    (
      validateId(args.headOption),
      validateName(args.lift(1)),
      validateDate(args.lift(2)),
      validateDate(args.lift(3))
    )
      .mapN[Computer](Computer.apply)
  }

  def validateId(idCadndidate: Option[String]): ValidatedNel[Exception, Long] = {
    idCadndidate
      .toRight(new IllegalArgumentException("Id should be defined"))
      .flatMap(id =>
        try Right[Exception, Long](id.toLong)
        catch {
          case NonFatal(_) => Left(new IllegalArgumentException("Id should be a long"))
        }
      )
      .toValidatedNel
  }

  def validateName(nameCandidate: Option[String]): ValidatedNel[Exception, String] = {
    nameCandidate
      .toRight(new IllegalArgumentException("Name should not be blank or empty"))
      .toValidatedNel
  }

  def validateDate(date: Option[String]): ValidatedNel[Exception, Option[LocalDate]] = {
    if (date.isEmpty) None.validNel
    else
      Try
        .apply(LocalDate.parse(date.fold("")(it => it)))
        .toEither
        .map(it => Option.apply(it))
        .left
        .map(_ => new IllegalArgumentException("Date should be defined as YYYY-MM-DD"))
        .toValidatedNel
  }
}

sealed trait ComputerCommand

case object ListComputers extends ComputerCommand

case class FetchComputer(id: Long) extends ComputerCommand

case class InsertComputer(computer: Computer) extends ComputerCommand
