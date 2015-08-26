// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow.builder

import ducttape.syntax.AbstractSyntaxTree.BranchGraftElement
import ducttape.workflow.Branch
import ducttape.workflow.BranchFactory
import ducttape.workflow.BranchPoint
import ducttape.workflow.BranchPointFactory
import org.scalatest.WordSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BranchGraftGlobTest extends WordSpec {

  private def assertEqual(a: Seq[BranchGraftElement], b: Seq[BranchGraftElement]) {

    import scala.collection.immutable.HashSet

    assert ( HashSet(a) == HashSet(b) )

  }

  val branchPointFactory = new BranchPointFactory
  val branchFactory = new BranchFactory(branchPointFactory)

  val branchesPerBranchPoint = 3
  
  // Define some branches
  Seq("a","b","c").foreach( x => {
    1.to(branchesPerBranchPoint).foreach( i => {
      branchFactory.get(s"${x}${i}",x.toUpperCase(),isBaseline=(i==1))
    })
  })  


  "A branch graft with no globs" should {

    "expand to itself" in {
      val unexpandedBranchGraftElements = Seq(
          new BranchGraftElement("A","a1"),
          new BranchGraftElement("B","b1"),
          new BranchGraftElement("C","c1")
      )

      val expandedBranchGraftElements = BranchGraftGlob.expand(unexpandedBranchGraftElements, branchPointFactory, branchFactory)

      expectResult(1)(expandedBranchGraftElements.size)

      assertEqual(unexpandedBranchGraftElements, expandedBranchGraftElements.head)

    }
  }
  
  
  "A branch graft with one glob" should {

    "expand" in {
      val unexpandedBranchGraftElements = Seq(
          new BranchGraftElement("A","*"),
          new BranchGraftElement("B","b1"),
          new BranchGraftElement("C","c1")
      )

      val expandedBranchGraftElements = BranchGraftGlob.expand(unexpandedBranchGraftElements, branchPointFactory, branchFactory)

      expectResult(branchesPerBranchPoint)(expandedBranchGraftElements.size)

    }
  }  

  "A branch graft with two globs" should {

    "expand" in {
      val unexpandedBranchGraftElements = Seq(
          new BranchGraftElement("A","*"),
          new BranchGraftElement("B","b1"),
          new BranchGraftElement("C","*")
      )

      val expandedBranchGraftElements = BranchGraftGlob.expand(unexpandedBranchGraftElements, branchPointFactory, branchFactory)

      expectResult(branchesPerBranchPoint*branchesPerBranchPoint)(expandedBranchGraftElements.size)

    }
  }    
  
  "A branch graft with all globs" should {

    "expand" in {
      val unexpandedBranchGraftElements = Seq(
          new BranchGraftElement("A","*"),
          new BranchGraftElement("B","*"),
          new BranchGraftElement("C","*")
      )

      val expandedBranchGraftElements = BranchGraftGlob.expand(unexpandedBranchGraftElements, branchPointFactory, branchFactory)

      expectResult(Math.pow(branchesPerBranchPoint, 3))(expandedBranchGraftElements.size)

    }
  }    
  
}