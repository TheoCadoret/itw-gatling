package io.gatling.interview.command

object ComputerCommand {
  def parse(args: List[String]): Option[ComputerCommand] =
    args match {
      case "list" :: _ => Some(ListComputers)
      case "find" :: a :: _ if a.toLongOption.isDefined => Some(FetchComputer(a.toLong))
      case _ => None
    }
}

sealed trait ComputerCommand

case object ListComputers extends ComputerCommand

case class FetchComputer(id: Long) extends ComputerCommand
