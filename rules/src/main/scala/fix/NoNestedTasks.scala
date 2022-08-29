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

  override def fix(implicit doc: SemanticDocument): Patch = {
    println("=================> Tree.structure: " + doc.tree.structureLabeled)
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
      // case  b @ Term.Block(args) => 
      //     println(s"====================== blocky1: (${args.structure})")           
      //     val bSymbol: Symbol = b.symbol
      //     val symbolInfo = doc.info(bSymbol)
      //     val syn = b.synthetics

      //     println(s"====================== blocky2: (${symbolInfo}) ${syn}")           

      //     val first = args.head
      //     val firstSymbol: Symbol = first.symbol
      //     val firstSymbolInfo = doc.info(firstSymbol)
      //     val owner = firstSymbolInfo.map(_.owner)
      //     val syntx = first.syntax

      //     firstSymbolInfo match {
      //       case Some(si) => 
      //         si.signature match {
      //           case MethodSignature(_, params, TypeRef(prefix, sym, ta)) => println(s"====================== blocky3: signature_class=(${si.signature.getClass} prefix=${prefix} symbol:${sym} symbol_display_name:${sym.displayName} ${ta} params=${params} owner=${owner} symbol_info:${sym.info} syntax=$syntx")           
      //           case other => println(s"====================== blocky5: (${other.getClass})")           
      //         }
                   
      //       case None => println(s"====================== blocky6: (${firstSymbol}), ${firstSymbolInfo.getClass}")           
      //     }
          
          
      //     Patch.empty



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
    }.asPatch
  }
}