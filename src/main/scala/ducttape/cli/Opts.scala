package ducttape.cli

import com.frugalmechanic.optparse.OptParse
import java.io.File
  
class Opts(conf: Config, args: Seq[String]) extends OptParse {
 
  class Mode(val name: String, val desc: String) extends OptParse {
    def optCount = allOpts.size
    def unapply(name: String) = if(name == this.name) Some(name) else None
  }
  
  //override val optParseDebug = true

  // TODO: Do some reflection and object apply() magic on modes to enable automatic subtask names
  val exec = new Mode("exec", desc="Execute the workflow (default if no mode is specified)") {
  }
  val jobs = {
    // TODO: XXX: Defaults seem to be *BROKEN*
    if (args.exists { arg => arg.startsWith("-j") || args.startsWith("--jobs")} ) {
      IntOpt(desc="Number of concurrent jobs to run")  
    } else {
      IntOpt(desc="Number of concurrent jobs to run", default=1)
    }
  }
  val config_file = StrOpt(desc="Stand-off workflow configuration file to read", short='C')
  val config_name = StrOpt(desc="Workflow configuration name to run", short='c', invalidWith=config_file)
  val yes = BoolOpt(desc="Don't prompt or confirm actions. Assume the answer is 'yes' and just do it.")
  val no_color = BoolOpt(desc="Don't colorize output")
  
  val list = new Mode("list", desc="List the tasks and realizations defined in the workflow");
  val env = new Mode("env", desc="Show the environment variables that will be used for a task/realization");
  val explain = new Mode("explain", desc="Explains why realizations get filtered out by a given plan");
  val viz = new Mode("viz", desc="Output a GraphViz dot visualization of the unpacked workflow");
  val debug_viz = new Mode("debug_viz", desc="Output a GraphViz dot visualization of the packed MetaHyperDAG");
  val mark_done = new Mode("mark_done", desc="Mark a specific task/realization as complete (useful if some manual recovery or resumption was necessary)");
  val invalidate = new Mode("invalidate", desc="Mark a specific task/realization and all of its children as invalid -- they won't be deleted, but they will be re-run with the latest version of your code and data");
  val purge = new Mode("purge", desc="Permenantly delete a specific task/realization and all of its children (recommend purging instead)");

  val modes = Seq(exec, list, env, viz, debug_viz, mark_done, invalidate)

  // Positional arguments:
  private var _workflowFile = new File(".")
  def workflowFile = _workflowFile

  private var _mode = "exec"
  def mode = _mode

  private var _taskName: Option[String] = None
  def taskName: Option[String] = _taskName

  private var _realNames: Seq[String] = Nil
  def realNames: Seq[String] = _realNames

  // TODO: Can we define help as an option?
  // TODO: Rewrite arg parsing as a custom module?
  override def help {
    System.err.println("Usage: ducttape workflow.tape [--options] [mode [taskName [realizationNames...]]]")
    System.err.println("Available modes: %s (default) %s".format(modes.head.name, modes.drop(1).map(_.name).mkString(" ")))
    super.help

    for (mode <- modes) {
      // TODO: Change visibility of init to protected instead of this hack...
      mode.parse(Array())

      if(mode.optCount > 1) {
        System.err.println("%s%s mode:%s".format(conf.modeColor, mode.name, conf.resetColor))
        mode.help
      }
    }
  }

  // TODO: Move to ErrorUtils?
  def exitHelp(msg: String, code: Int) {
    help
    System.err.println("%sERROR: %s%s".format(conf.errorColor, msg, conf.resetColor))
    System.exit(code)
  }

  if (args.isEmpty || args(0).startsWith("-")) {
    exitHelp("Workflow file is required", 1)
  }

  _workflowFile = new File(args(0))

  private val leftoversOpt = defaultOpt(MultiStrOpt())
  parse(args.drop(1).toArray) // skip workflow file
  private val posArgs = leftoversOpt.getOrElse(Nil)
  // TODO: More general positional args parsing
  if (posArgs.size >= 1)
    _mode = posArgs(0)
  if (posArgs.size >= 2)
    _taskName = Some(posArgs(1))
  if (posArgs.size >= 3)
    _realNames = posArgs.drop(2)
}