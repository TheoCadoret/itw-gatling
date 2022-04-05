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

  def fetch(id: Long): F[Option[Computer]] = {
    fetchAll()
      .map(computers =>
        computers.find(computer => computer.id.equals(id)))
  }

  def insert(newComputer: Computer): F[Unit] = {
    fetchAll()
      .map(computers => computers.appended(newComputer))
      .map(computers => computers.asJson)
      .flatMap(json => blocker.blockOn(F.delay {
        Files.write(filePath, json.toString().getBytes(ComputersFileCharset))
      }))
  }
}
