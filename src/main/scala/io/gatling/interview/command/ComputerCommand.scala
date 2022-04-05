package io.gatling.interview.command

import io.gatling.interview.model.Computer

import java.time.LocalDate
import scala.util.Try
import scala.util.control.NonFatal

object ComputerCommand {
  def parse(args: List[String]): Option[ComputerCommand] =
    args match {
      case "list" :: _ => Some(ListComputers)
      case "find" :: a :: _ => a.toLongOption.map(it => FetchComputer(it))
      case "insert" :: tail => ComputorValidator.validateComputer(tail).map(it => InsertComputer(it)).toOption
      case _ => None
    }
}

object ComputorValidator {
  def validateComputer(args: List[String]): Either[Exception, Computer] = {
    validateId(args.headOption)
      .flatMap(id => validateName(args.lift(1))
        .flatMap(name => validateDate(args.lift(2))
          .flatMap(introduced => validateDate(args.lift(3))
            .map(discontinued =>
              Computer(id, name, introduced, discontinued)
            ))))
  }

  def validateId(idCadndidate: Option[String]): Either[Exception, Long] = {
    idCadndidate.toRight(new IllegalArgumentException("Id should be defined"))
      .flatMap(it => try Right[Exception, Long](it.toLong) catch {
        case NonFatal(_) => Left(new IllegalArgumentException("Id should be a long"))
      })
  }

  def validateName(nameCandidate: Option[String]): Either[Exception, String] = {
    nameCandidate.toRight(new IllegalArgumentException("Name should not be blank or empty"))
  }

  def validateDate(date: Option[String]): Either[Exception, Option[LocalDate]] = {
    if (date.isEmpty) Right(None)
    else Try.apply(LocalDate.parse(date.fold("")(it => it)))
      .toEither
      .map(it => Option.apply(it))
      .left.map(_ => new IllegalArgumentException("Date should be defined as YYYY-MM-DD"))
  }
}

sealed trait ComputerCommand

case object ListComputers extends ComputerCommand

case class FetchComputer(id: Long) extends ComputerCommand

case class InsertComputer(computer: Computer) extends ComputerCommand
