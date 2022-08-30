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
