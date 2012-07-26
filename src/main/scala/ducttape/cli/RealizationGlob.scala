package ducttape.cli

import ducttape.workflow.Branch
import ducttape.workflow.Realization
import ducttape.util.Globs

import java.util.regex.Pattern

class RealizationGlob(glob: String) {
  // the pattern contains one tuple per branch point specified within the glob
  // the tuple consists of a branch point name glob and a branch glob

  val globElems: Seq[String] = glob.split('-').toSeq

  // branch point name -> pattern
  val patterns: Map[String,Pattern] = {
    globElems.filter(_ != "*").map { elem: String =>
      if (elem.count(_ == '.') != 1) {
        // TODO: Not a runtime exception?
        throw new RuntimeException("Realization glob element must have exactly one dot to separate branch point and branch: " + elem)
      }
      val Array(branchPoint, branchGlob) = elem.split("\\.")

      System.err.println("GLOB: %s ==> %s".format(branchPoint, branchGlob))

      // we explicitly disallow globbing within branch names to keep the semantics of
      // branch globbing easy to understand

      val branchPat = Pattern.compile(Globs.globToRegex(branchGlob))
      (branchPoint, branchPat)
    }
  }.toMap

  // if the user specifies the "global glob", it indicates that ducttape
  // should glob *all* branches of branch points that were not otherwise specified.
  // without the "global glob", ducttape will take only the baseline realization of
  // branch points that are not explicitly mentioned
  val globalGlob: Boolean = globElems.contains("*")

  def matches(real: Realization): Boolean = {
    real.branches.forall { branch: Branch =>
      val branchOk = patterns.get(branch.branchPoint.name) match {
        // didn't match any explicitly mentioned branch points
        case None => globalGlob || branch.baseline
        // matched a branch point... what about the branch?
        case Some(branchPat) => {
          System.err.println("Matching %s against %s".format(branch.toString, branchPat.toString))
          branchPat.matcher(branch.name).matches
        }
      }
      if (!branchOk) {
        System.err.println("Branch not matched: " + branch)
      }
      branchOk
    }
  }
}
