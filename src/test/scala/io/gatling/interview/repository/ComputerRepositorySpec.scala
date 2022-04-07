package io.gatling.interview.repository

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{Blocker, IO, Resource}
import io.circe.ParsingFailure
import io.gatling.interview.model.Computer
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.file.{Files, Path, StandardCopyOption}
import java.time.{LocalDate, Month}
import java.util.UUID

class ComputerRepositorySpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  private val blocker = Blocker.liftExecutionContext(executionContext)
  val initialComputerList = Seq(
    Computer(id = 1, name = "MacBook Pro 15.4 inch", introduced = None, discontinued = None),
    Computer(
      id = 2,
      name = "CM-5",
      introduced = Some(LocalDate.of(1991, Month.JANUARY, 1)),
      discontinued = None
    ),
    Computer(
      id = 3,
      name = "Apple IIee",
      introduced = Some(LocalDate.of(2006, Month.JANUARY, 10)),
      discontinued = Some(LocalDate.of(2010, Month.JANUARY, 10))
    )
  )
  "ComputerRepository#fetchAll" should "retrieve all computers" in {
    val expectedComputers = initialComputerList

    temporaryFileResource("computers/computers.json")
      .use { computersFilePath =>
        val repository = new ComputerRepository[IO](computersFilePath, blocker)
        repository.fetchAll()
      }
      .asserting { fetchedComputers =>
        fetchedComputers shouldBe expectedComputers
      }
  }

  "ComputerRepository#fetchOne" should "retrieve one computer" in {
    val expectedComputer =
      Computer(
        id = 3,
        name = "Apple IIee",
        introduced = Some(LocalDate.of(2006, Month.JANUARY, 10)),
        discontinued = Some(LocalDate.of(2010, Month.JANUARY, 10))
      )

    temporaryFileResource("computers/computers.json")
      .use { computersFilePath =>
        val repository = new ComputerRepository[IO](computersFilePath, blocker)
        repository.fetchOne(3)
      }
      .asserting { fetchedComputer =>
        fetchedComputer shouldBe expectedComputer
      }
  }

  "ComputerRepository#fetchOne" should "fail when id not found" in {
    temporaryFileResource("computers/computers.json")
      .use { computersFilePath =>
        val repository = new ComputerRepository[IO](computersFilePath, blocker)
        repository.fetchOne(10)
      }
      .assertThrows[IllegalArgumentException]
  }

  "ComputerRepository#insert" should "insert the new computer" in {
    val newComputer = Computer(
      id = 4,
      name = "MSI",
      introduced = Some(LocalDate.of(2006, Month.JANUARY, 10)),
      discontinued = Some(LocalDate.of(2010, Month.JANUARY, 10))
    )
    val expectedComputers = initialComputerList.appended(newComputer)

    temporaryFileResource("computers/computers.json")
      .use { computersFilePath =>
        val repository = new ComputerRepository[IO](computersFilePath, blocker)
        for {
          _ <- repository.insert(newComputer)
          computers <- repository.fetchAll()
        } yield computers
      }
      .asserting { fetchedComputers =>
        fetchedComputers shouldBe expectedComputers
      }
  }

  "ComputerRepository#insert" should "fail if Id already exist in db" in {
    val newComputer = Computer(
      id = 2,
      name = "MSI",
      introduced = Some(LocalDate.of(2006, Month.JANUARY, 10)),
      discontinued = Some(LocalDate.of(2010, Month.JANUARY, 10))
    )
    temporaryFileResource("computers/computers.json")
      .use { computersFilePath =>
        val repository = new ComputerRepository[IO](computersFilePath, blocker)
        repository.insert(newComputer)
      }
      .assertThrows[IllegalArgumentException]
  }

  "ComputerRepository#fetchAll" should "fail if the JSON file is invalid" in {
    temporaryFileResource("computers/computers-invalid.json")
      .use { computersFilePath =>
        val repository = new ComputerRepository[IO](computersFilePath, blocker)
        repository.fetchAll()
      }
      .assertThrows[ParsingFailure]
  }

  private def temporaryFileResource(path: String): Resource[IO, Path] =
    Resource(
      for {
        inputStream <- IO.delay(getClass.getClassLoader.getResourceAsStream(path))
        file <- IO.delay(File.createTempFile(UUID.randomUUID().toString, "tmp"))
        path = file.toPath
        _ <- IO.delay(file.deleteOnExit())
        _ <- IO.delay(Files.copy(inputStream, path, StandardCopyOption.REPLACE_EXISTING))
      } yield (path, IO.delay(file.delete()).as(()))
    )
}
