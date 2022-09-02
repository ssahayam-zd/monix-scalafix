/*
rule = NoNestedTasks
*/
package fix

import monix.eval.Task

object NoNestedTasks {

  val nested = Task(Task(1)) /* assert: NoNestedTasks
                    ^^^^^^^
Nested Task found: You're wrapping a Task with a Task
  */

  val nestedPure = Task.pure(Task("not so pure")) /* assert: NoNestedTasks
                             ^^^^^^^^^^^^^^^^^^^
Nested Task found: You don't need to use `pure` to wrap a Task. A Task is pure already
  */


  def log(message: String): Task[Unit] = Task(println(s"logged: $message")) // ok

  def msg(message: String): Task[String] = Task.pure(message) // ok


  def nestedLog(): Task[Unit] = {
    Task(log("hello")) /* assert: NoNestedTasks
         ^^^^^^^^^^^^
Nested Task found: You don't need to wrap the function: `log` in Task because it already returns Task
  */
  }

  def simpleLog(): Task[Unit] = {
    log("hello")
  }

  def shouldHaveFlatMapped(): Task[Unit] = {
    log("first log").map(_ =>  log("second log")) /* assert: NoNestedTasks
                         ^^^^^^^^^^^^^^^^^^^^^^^              
Nested Task found: Try `flatMap` instead of `map`
  */
  }

 
  // Basic for-comp with functions that return Task
  def forCompShouldHaveFlatMapped(): Task[Unit] = {
    for {
      _ <- log("first log")
      _ <- log("second log").map(_ => Task(println("third"))) /* assert: NoNestedTasks
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^              
Nested Task found: Try `flatMap` instead of `map`
*/      
    } yield ()
  }

  // Function that returns Task followed by 
  // another Function that takes multiple parameters and returns Task with another function that returns Task
  def forCompShouldHaveFlatMapped2(): Task[Unit] = {
    val extra = Task("the end")
    for {
      _ <- log("first log")
      _ <- extendedLog("additional", Severity.Medium, "second log", extra).map(_ => log("third")) // assert: NoNestedTasks
    } yield ()
  }

  // Task() and Task followed by
  // Function that returns Task and Task()
  def forCompShouldHaveFlatMapped3(): Task[Unit] = {
    for {
      _ <- Task(1).map(_ => Task(2))  /* assert: NoNestedTasks
                       ^^^^^^^^^^^^              
Nested Task found: Try `flatMap` instead of `map`
  */

      _ <- log("second log").map(_ => Task(2)) /* assert: NoNestedTasks
                                 ^^^^^^^^^^^^              
Nested Task found: Try `flatMap` instead of `map`
  */

    } yield ()
  }

  // Task() followed by
  // Task() and function that returns Task
  def forCompShouldHaveFlatMapped4(): Task[Unit] = {
    for {
      _ <- Task(1)
      _ <- Task(2).map(_ => log("second log")) /* assert: NoNestedTasks
                       ^^^^^^^^^^^^^^^^^^^^^^              
Nested Task found: Try `flatMap` instead of `map`
  */
    } yield ()
  }

  // Mapping a Task val with a function that returns a Task
  def forCompShouldHaveFlatMapped5(): Task[Unit] = {
    val extra = Task("the end")
    for {
      _ <- log("first log").map(_ => extra) /* assert: NoNestedTasks
                                ^^^^^^^^^^              
Nested Task found: Try `flatMap` instead of `map`
  */
    } yield ()
  }

  // Mapping a function that returns a Task from a val Task
  def forCompShouldHaveFlatMapped6(): Task[Unit] = {
    val extra = Task("the end")
    for {
      _ <- extra.map(_ => log("first log")) /* assert: NoNestedTasks
                     ^^^^^^^^^^^^^^^^^^^^^             
Nested Task found: Try `flatMap` instead of `map`
  */
    } yield ()
  }

  sealed trait Severity

  object Severity {
    case object Low extends Severity
    case object Medium extends Severity
    case object High extends Severity    
  }

  private def extendedLog(context: String, severity: Severity, message: String, other: Task[String]): Task[Unit] = {
    other.flatMap(o => log(s"[$severity] $context: $message $o"))
  }

  def mapStillWorks(): Task[String] = {
    msg("hello").map(_.toUpperCase()) // ok
  }

  def flatMapStillWorks(): Task[String] = {
    msg("hello").flatMap(m => msg(s"$m there")) // ok
  }

  def flatMapStillWorksWithAnonymousFunction(): Task[String] = {
    msg("hello").flatMap(_ => msg("My new message")) // ok
  }
}
