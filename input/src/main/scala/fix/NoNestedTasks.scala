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
}
