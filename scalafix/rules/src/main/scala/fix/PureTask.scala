package fix

import scalafix.v1._
import scala.meta._

final case class PureTaskDianostic(term: Term, description: String) extends Diagnostic {
  override def position: Position = term.pos
  override def message: String = 
    s"Impure task found: $description"
}

class PureTask extends SemanticRule("PureTask") {

  val TASK_PURE_FUNC = SymbolMatcher.exact("monix/eval/Task.pure().")
  val UNIT_TYPE = SymbolMatcher.exact("scala/Unit#")

  override def fix(implicit doc: SemanticDocument): Patch = {
    // println("=================> Tree.structure: " + doc.tree.structureLabeled)
    doc.tree.collect {
      case  Term.Apply(
              Term.Select(_, TASK_PURE_FUNC(_)),
              (fa @ Term.Apply(fn, _)) :: _
            ) => 
              val symbol = fn.symbol
              val symbolInfo = symbol.info
            symbolInfo match {
              case Some(si) => 
                si.signature match {
                  case MethodSignature(_, _, TypeRef(prefix, UNIT_TYPE(_), _)) => 
                    Patch.lint(
                      PureTaskDianostic(
                        fa, 
                        s"side effecting functions should not be put into Task.pure\nTry `Task($fa)` instead"
                      )
                    )
                  case _ => Patch.empty
              }

              case None => Patch.empty
            }

      case  Term.Apply(
              Term.Select(_, TASK_PURE_FUNC(_)),
              (fa @ Term.Throw(_)) :: _
            ) =>  Patch.lint(PureTaskDianostic(fa, s"don't throw exceptions in Task.pure\nTry: `Task($fa)` instead"))
    }.asPatch
  }

}