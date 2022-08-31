/*
rule = PureTask
*/
package fix

import monix.eval.Task

object PureTask {

  val sideEffectInPure = Task.pure(println("hello")) /* assert: PureTask
                                   ^^^^^^^^^^^^^^^^
Impure task found: side effecting functions should not be put into Task.pure
*/
}
