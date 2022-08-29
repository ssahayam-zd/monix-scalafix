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

  override def fix(implicit doc: SemanticDocument): Patch = {
    doc.tree.collect {
      case  Term.Apply(TASK_M(x), (nested @ TASK_M(y)) :: _) =>
        println(s"====================== MATCHED!!! $x $y ${nested.structure}, ${nested.pos}") 
        Patch.lint(NestedTaskDiagnostic(nested))

      case  Term.Apply(TASK_M(x), (nested @ Term.Apply(arg, _)) :: _) =>
          println(s"====================== matched2: $x($arg)")             
          println("Tree.structureLabeledz2: " + arg.structureLabeled)
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
    }.asPatch
  }
}