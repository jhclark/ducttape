package ducttape.cli
// TODO: Move out of CLI

import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.FileFormatException

// may throw a FileFormatException
class Directives(confSpecs: Seq[ConfigAssignment]) {
  // TODO: Move conf specs method into WorkflowDefinition?
  def getLiteralSpec(name: String): Option[LiteralSpec] = {
    confSpecs.map(_.spec).find { spec => spec.name == name } match {
      case Some(spec) => spec.rval match {
        // silly typesystem doesn't detect that this is trivially true...
        case lit: Literal => Some(spec.asInstanceOf[LiteralSpec])
        case _ => throw new FileFormatException("%s directive must be a literal".format(name), spec)
      }
      case None => None
    }
  }
  def getLiteralSpecValue(name: String): Option[String] = getLiteralSpec(name).map(_.rval.value)
  
  val flat: Boolean = {
    getLiteralSpec("ducttape_structure") match {
      case Some(literalSpec) => literalSpec.rval.value.toLowerCase match {
        case "flat" => true
        case "hyper" => false
        case _ => throw new FileFormatException("ducttape_structure directive must be either 'flat' or 'hyper'", literalSpec)
      }
      case None => false // not flat by default (hyper)
    }
  }

  // use a separate directory inside each task-realization for each workflow version that is run?
  // TODO: false to maintain backward compatibility -- preferred is true
  val versionedTasks: Boolean = parse("ducttape_versioned_tasks", default=false)
  
  val output: Option[String] = getLiteralSpecValue("ducttape_output")

  val undeclared_vars: Option[String] = getLiteralSpecValue("ducttape_undeclared_vars")
  val unused_vars: Option[String] = getLiteralSpecValue("ducttape_unused_vars")
    
  // XXX: TODO: Remove as soon as Jon's current experiments are done - this is a hack
  getLiteralSpecValue("ducttape_branchpoint_delimiter") match {
    case Some(value) => ducttape.workflow.Realization.delimiter = value.toLowerCase.slice(0, 1)
    case None => ;
  }

  def parse(key: String, default: Boolean): Boolean = getLiteralSpecValue(key) match {
    case Some(value) => Booleans.parseBoolean(value)
    case None => default
  }

  def parseExperimental(key: String): Boolean = parse(key, default=false)

  // TODO: Identify unrecognized ducttape directives and error out?

  // do we check for new versions of software every time we run a workflow?
  // or only when we initiate a new workflow and then only on-demand?
  val autoUpdatePackages: Boolean = parseExperimental("ducttape_auto_update_packages")

  // experimental directives
  val enableImports: Boolean = parseExperimental("ducttape_experimental_imports")
  val enablePackages: Boolean = parseExperimental("ducttape_experimental_packages")
  val enableSubmitters: Boolean = parseExperimental("ducttape_experimental_submitters")

  // enable multiple ducttape processes to run at the same time
  // (this has nothing to do with parallel execution of tasks!)
  val enableMultiproc: Boolean = parseExperimental("ducttape_experimental_multiproc")
}
