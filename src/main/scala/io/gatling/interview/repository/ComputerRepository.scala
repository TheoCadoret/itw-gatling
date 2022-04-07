package io.gatling.interview.repository

import cats.effect.{Blocker, ContextShift, Sync}
import cats.implicits._
import io.circe.parser.decode
import io.circe.syntax._
import io.gatling.interview.model.Computer
import io.gatling.interview.repository.ComputerRepository.ComputersFileCharset

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path, Paths}

object ComputerRepository {
  val DefaultComputersFilePath: Path = Paths.get("computers.json")
  private val ComputersFileCharset: Charset = StandardCharsets.UTF_8
}

class ComputerRepository[F[_]: ContextShift](filePath: Path, blocker: Blocker)(implicit
    F: Sync[F]
) {

  def fetchAll(): F[Seq[Computer]] =
    for {
      json <- blocker.blockOn(F.delay {
        val jsonBytes = Files.readAllBytes(filePath)
        new String(jsonBytes, ComputersFileCharset)
      })
      computers <- F.fromEither(decode[Seq[Computer]](json))
    } yield computers

  def fetchOne(id: Long): F[Computer] = {
    for {
      computers <- fetchAll()
      computer <- computers
        .find(computer => computer.id.equals(id))
        .fold(
          F.raiseError[Computer](
            new IllegalArgumentException(s"No computer matching given id : ${id.toString}")
          )
        )(it => F.pure(it))
    } yield computer
  }

  def insert(newComputer: Computer): F[Unit] = {
    for {
      computers <- fetchAll()
      computers <- computers
        .find(computer => computer.id.equals(newComputer.id))
        .fold(
          F.pure(computers)
        )(_ =>
          F.raiseError[Seq[Computer]](
            new IllegalArgumentException(
              s"A computer with id ${newComputer.id.toString} already exists"
            )
          )
        )
        .map(computers => computers.appended(newComputer))
        .map(computers => computers.asJson)
      _ <- blocker.blockOn(F.delay {
        Files.write(filePath, computers.toString().getBytes(ComputersFileCharset))
      })
    } yield ()
  }
}
