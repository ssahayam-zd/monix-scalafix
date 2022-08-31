/*
rule = PureTask
*/
package fix

import monix.eval.Task
import scala.collection.mutable.ListBuffer

object PureTask {

  val sideEffectInPure1 = Task.pure(println("hello")) /* assert: PureTask
                                    ^^^^^^^^^^^^^^^^
Impure task found: side effecting functions should not be put into Task.pure
Try `Task(println("hello"))` instead
*/

  def logInfo(message: String): Unit = println(message)

  val sideEffectInPure2 = Task.pure(logInfo("hello")) /* assert: PureTask
                                    ^^^^^^^^^^^^^^^^
Impure task found: side effecting functions should not be put into Task.pure
Try `Task(logInfo("hello"))` instead
*/

  val lb = ListBuffer("one")

  val sideEffectInPure3 = Task.pure(lb.insert(1, "hello")) /* assert: PureTask
                                    ^^^^^^^^^^^^^^^^^^^^^
Impure task found: side effecting functions should not be put into Task.pure
Try `Task(lb.insert(1, "hello"))` instead
*/

  val throwInPure = Task.pure(throw new RuntimeException("boom!")) /* assert: PureTask
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Impure task found: don't throw exceptions in Task.pure
Try: `Task(throw new RuntimeException("boom!"))` instead
 */ 

 val validPure1 = Task.pure(1) // ok

 def greet(name: String): String = s"hello $name"
 
 val validPure2 = Task.pure(greet("Fred")) // ok
}
