package ducttape.workflow.builder

import ducttape.syntax.AbstractSyntaxTree.BranchGraftElement
import ducttape.workflow.Branch
import ducttape.workflow.BranchFactory
import ducttape.workflow.BranchPointFactory

object BranchGraftGlob {
  
  def expand(
      branchGraftElements: Seq[BranchGraftElement],
      branchPointFactory: BranchPointFactory, 
      branchFactory: BranchFactory,
      accumulator: Seq[BranchGraftElement] = Seq.empty)
    : Seq[Seq[BranchGraftElement]] = {
        
    branchGraftElements.headOption match {
      case None                     => return Seq(accumulator)
      case Some(branchGraftElement) => {
        if (branchGraftElement.isGlob) {
          val branches = branchFactory.getAll(branchGraftElement.branchPointName).toSeq
          return branches.flatMap( (branch: Branch) => {
            val expandedBranchGraftElement = new BranchGraftElement(branchGraftElement.branchPointName, branch.toString)
            expand(branchGraftElements.tail, branchPointFactory, branchFactory, accumulator++Seq(expandedBranchGraftElement)) 
          })
        } else {
          return expand(branchGraftElements.tail, branchPointFactory, branchFactory, accumulator++Seq(branchGraftElement))
        }
      }
    }
    
  }  
  
}