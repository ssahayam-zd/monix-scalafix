/*
rule = NoNestedTasks
*/
package fix

import monix.eval.Task

object NoNestedTasks {

  val nested = Task(Task(1)) /* assert: NoNestedTasks
                    ^^^^^^^
Nested Task found
  */

  def log(message: String): Task[Unit] = Task(println(s"logged: $message")) // ok

  def nestedLog(): Task[Unit] = {
    Task(log("hello")) /* assert: NoNestedTasks
         ^^^^^^^^^^^^
Nested Task found
  */
  }

  def simpleLog(): Task[Unit] = {
    log("hello")
  }

  def shouldHaveFlatMapped(): Task[Unit] = {
    log("first log").map(_ =>  log("second log")) /* assert: NoNestedTasks
                         ^^^^^^^^^^^^^^^^^^^^^^^              
Nested Task found
  */

  }
}
