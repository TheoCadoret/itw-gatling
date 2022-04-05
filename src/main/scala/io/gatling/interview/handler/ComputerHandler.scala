package io.gatling.interview.handler

import cats.effect.Sync
import cats.implicits._
import io.gatling.interview.command.{ComputerCommand, FetchComputer, InsertComputer, ListComputers}
import io.gatling.interview.console.Console
import io.gatling.interview.repository.ComputerRepository

class ComputerHandler[F[_]](computerRepository: ComputerRepository[F], console: Console[F])(implicit
    F: Sync[F]
) {

  def handle(command: ComputerCommand): F[Unit] =
    command match {
      case ListComputers =>
        for {
          computers <- computerRepository.fetchAll()
          output = computers
            .map { c => c.toString }
            .mkString("\n")
          _ <- console.println(output)
        } yield ()
      case FetchComputer(id) =>
        for {
          computer <- computerRepository.fetch(id)
          output = computer.fold("No computer found")(c => c.toString)
          _ <- console.println(output)
        } yield ()
      case InsertComputer(computer) =>
        for {
          _ <- computerRepository.insert(computer)
          _ <- console.println(s"Computer ${computer.toString} has been added")
        } yield ()
    }
}
