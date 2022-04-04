package io.gatling.interview.model

import io.circe._
import io.circe.generic.semiauto._

import java.time.LocalDate

object Computer {
  implicit val decoder: Decoder[Computer] = deriveDecoder
  implicit val encoder: Encoder[Computer] = deriveEncoder
}

final case class Computer(
                           id: Long,
                           name: String,
                           introduced: Option[LocalDate],
                           discontinued: Option[LocalDate]
                         ) {
  override def toString: String = {
    val introducedStr = introduced.map(d => s", introduced: ${d.toString}").getOrElse("")
    val discontinuedStr = discontinued.map(d => s", discontinued: ${d.toString}").getOrElse("")
    s"- [${id.toString}] ${name}$introducedStr$discontinuedStr"
  }
}
