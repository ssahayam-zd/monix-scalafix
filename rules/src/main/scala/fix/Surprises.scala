package fix

import scalafix.v1._
import scala.meta._

class Surprises extends SemanticRule("Surprises") {

  val TASK_M = SymbolMatcher.exact("monix/eval/Task.")
  val TASK_R = SymbolMatcher.exact("monix/eval/Task#")

  override def fix(implicit doc: SemanticDocument): Patch = {
    // println("Tree.syntax: " + doc.tree.syntax)
    // println("Tree.structure: " + doc.tree.structure)
    println("Tree.structureLabeledz: " + doc.tree.structureLabeled)
    

    doc.tree.collect {
      case  Term.Apply(TASK_M(x), TASK_M(y) :: _) =>
        // val symb = tn.symbol
        println(s"====================== matched1: $x, $y")
 
        // symb match {
        //   case TASK_M(_) => println("############### matched")
        //   case _ => println("############### not matched")
        // }
        Patch.empty

      case  Term.Apply(TASK_M(x), Term.Apply(arg, _) :: _) =>
          println(s"====================== matched2: $x($arg)")             
          println("Tree.structureLabeledz2: " + arg.structureLabeled)
          val aSymbol: Symbol = arg.symbol

          val symbolInfo = doc.info(aSymbol)

          symbolInfo match {
            case Some(si) => 
              si.signature match {
                case MethodSignature(_, _, TypeRef(prefix, TASK_R(_), _)) =>  println(s"======================> method2-1 $prefix Monix")                             
                case MethodSignature(_, _, TypeRef(prefix, sym, rt)) =>  println(s"======================> method2-2 $x $prefix $sym $rt")                             
                case MethodSignature(_, _, rt) =>  println(s"======================> method2-3 $x rt: ${rt.getClass}")                             
                case _ => println(s"======================> not a method")             
              }
                
            case _ => println(s"======================> no symbol info")             
          }           
          Patch.empty
      case _ =>  Patch.empty
    }
    .asPatch
  }

}
