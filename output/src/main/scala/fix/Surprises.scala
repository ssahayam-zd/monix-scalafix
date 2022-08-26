package fix

import monix.eval.Task

object Surprises {

  val nested = Task(Task(1))

  def log(message: String): Task[Unit] = Task(println(s"logged: $message"))

  def otherFunct(): Task[Unit] = {
    Task(log("hello"))
  }

  def otherFunct2(): Task[Unit] = {
    log("hello")
  }

}


