package app6.log

import cats.{Id, ~>}

object interpreter {

  import dsl._

  sealed trait LogLevel
  case object INFO extends LogLevel
  case object WARN extends LogLevel
  case object ERROR extends LogLevel

  private def logOutput(level: LogLevel, message: String): String = {
    val now = new java.util.Date().toString
    s"##### $level: $now - $message"
  }

  object LogInterpreter extends (Log ~> Id) {

    override def apply[A](fa: Log[A]): Id[A] = fa match {
      case Info(msg) =>
        println(logOutput(INFO, msg))
      case Warn(msg) =>
        println(logOutput(WARN, msg))
      case Error(msg) =>
        println(logOutput(ERROR, msg))
    }
  }
}
