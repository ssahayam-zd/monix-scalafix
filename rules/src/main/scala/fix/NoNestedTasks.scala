package fix

import scalafix.v1._
import scala.meta._

final case class NestedTaskDiagnostic(term: Term) extends Diagnostic {
  override def position: Position = term.pos
  override def message: String = 
    "Nested Task found"
}

class NoNestedTasks extends SemanticRule("NoNestedTasks") {

  val TASK_M = SymbolMatcher.exact("monix/eval/Task.")
  val TASK_R = SymbolMatcher.exact("monix/eval/Task#")
  val TASK_MAP_FUNC = SymbolMatcher.exact("monix/eval/Task#map().")
  val TASK_PURE_FUNC = SymbolMatcher.exact("monix/eval/Task.pure().")

  override def fix(implicit doc: SemanticDocument): Patch = {
    // println("=================> Tree.structure: " + doc.tree.structureLabeled)
    doc.tree.collect {
      case  Term.Apply(TASK_M(x), (nested @ TASK_M(y)) :: _) =>
        Patch.lint(NestedTaskDiagnostic(nested))

      case  Term.Apply(TASK_M(x), (nested @ Term.Apply(arg, _)) :: _) =>
          val aSymbol: Symbol = arg.symbol

          val symbolInfo = doc.info(aSymbol)

          symbolInfo match {
            case Some(si) => 
              si.signature match {
                case MethodSignature(_, _, TypeRef(prefix, TASK_R(_), _)) =>  Patch.lint(NestedTaskDiagnostic(nested))
                case _ => Patch.empty
              }
                
            case _ => Patch.empty
          }
          
      case Term.Apply(
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
                Patch.lint(NestedTaskDiagnostic(mf))
              case _ => Patch.empty
            }

    case  Term.Apply(
            Term.Select(_, TASK_PURE_FUNC(_)),
            (nested @ Term.Apply(TASK_M(_), _)) :: _
          ) => Patch.lint(NestedTaskDiagnostic(nested))

    }.asPatch
  }
}