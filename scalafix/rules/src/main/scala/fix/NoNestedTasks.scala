package fix

import scalafix.v1._
import scala.meta._

final case class NestedTaskDiagnostic(term: Term, description: String) extends Diagnostic {
  override def position: Position = term.pos
  override def message: String = 
    s"Nested Task found: $description"
}

class NoNestedTasks extends SemanticRule("NoNestedTasks") {

  val TASK_M = SymbolMatcher.exact("monix/eval/Task.")
  val TASK_R = SymbolMatcher.exact("monix/eval/Task#")
  val TASK_MAP_FUNC = SymbolMatcher.exact("monix/eval/Task#map().")
  val TASK_PURE_FUNC = SymbolMatcher.exact("monix/eval/Task.pure().")

  override def fix(implicit doc: SemanticDocument): Patch = {
    // println("=================> Tree.structure: " + doc.tree.structureLabeled)
    doc.tree.collect {
      // Example: Creating a Task with a Task: Task(Task(???))
      case  Term.Apply(TASK_M(x), (nested @ TASK_M(y)) :: _) =>
        Patch.lint(NestedTaskDiagnostic(nested, "You're wrapping a Task with a Task"))

      // Example: wrapping a function that returns a Task in a Task: Task(taskFunction)
      case  Term.Apply(TASK_M(x), (nested @ Term.Apply(arg, _)) :: _) =>
          val aSymbol: Symbol = arg.symbol

          val symbolInfo = doc.info(aSymbol)

          symbolInfo match {
            case Some(si) => 
              si.signature match {
                case MethodSignature(_, _, TypeRef(prefix, TASK_R(_), _)) => Patch.lint(NestedTaskDiagnostic(nested, s"You don't need to wrap the function: `${aSymbol.displayName}` in Task because it already returns Task"))
                case _ => Patch.empty
              }
                
            case _ => Patch.empty
          }

      // TODO: We might need this for a corner case. Delete if we can prove otherwise
      // Example: mapping over a Task when you should have flatMapped: Task(???).map(_ => Task(???))
      // case abc @ Term.Apply(
      //       Term.Select(
      //         Term.Apply(Term.Name("Task"), _),
      //         Term.Name("map")
      //       ),
      //       List(
      //         Term.Function(
      //           _,
      //           Term.Apply(Term.Name("Task"), _)
      //         )
      //       )
      //     ) =>  Patch.lint(NestedTaskDiagnostic(abc, "1 Try `flatMap` instead of `map`"))

    // Example: mapping over a Task when you should have flatMapped: log(???).map(_ => Task(???))
     case xyz @ Term.Apply(
            Term.Select(
              _,
              TASK_MAP_FUNC(n)
            ),
            List(
              mf @ Term.Function(
                _,
                Term.Apply(
                   TASK_M(_),
                   _
                )
              )
            )
          ) => 
              Patch.lint(NestedTaskDiagnostic(mf, "Try `flatMap` instead of `map`"))

      // Example: mapping over a Task when you should have flatMapped: Task(???).map(_ => Task(???))
      case xyz @ Term.Apply(
            Term.Select(
              _,
              TASK_MAP_FUNC(n)
            ),
            List(
              mf @ Term.Function(
                _,
                body
              )
            )
          ) =>
            val bodySymbol = body.symbol
            val bodySymbolInfo = bodySymbol.info
            val bodySig = bodySymbolInfo.map(_.signature)
            
            bodySig match {
              case Some(MethodSignature(_, _, TypeRef(_, TASK_R(_), _))) => 
                Patch.lint(NestedTaskDiagnostic(mf, "Try `flatMap` instead of `map`"))


              /** Example: Mapping a Task val with a function that returns a Task:
               * val someTask = Task[Int]
               * for {
               *   _ <- logInfo("whatever").map(_ => someTask)
               * } yield ()
               */
              case Some(ValueSignature(TypeRef(_, TASK_R(_), _))) => 
                Patch.lint(NestedTaskDiagnostic(mf, "Try `flatMap` instead of `map`"))
              case _ => 
                Patch.empty
            }


    // Example: Create Task inside Task.pure: Task.pure(Task(???))
    case  Term.Apply(
            Term.Select(_, TASK_PURE_FUNC(_)),
            (nested @ Term.Apply(TASK_M(_), _)) :: _
          ) => Patch.lint(NestedTaskDiagnostic(nested, "You don't need to use `pure` to wrap a Task. A Task is pure already"))

    }.asPatch
  }
}