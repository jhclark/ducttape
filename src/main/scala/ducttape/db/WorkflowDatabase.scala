package ducttape.db

import java.sql._
import collection._
import java.io.File

class PackageInfo(val name: String, val version: String);
class InputInfo(val name: String, val path: File, val srcTaskAndReal: Option[(String, String)]);
class OutputInfo(val name: String, val path: File);
class ParamInfo(val name: String, val value: String, val srcTaskAndReal: Option[(String, String)]);

class TaskInfo(
  val name: String,
  val realName: String,
  val status: String,
  val packages: Seq[PackageInfo],
  val inputs: Seq[InputInfo],
  val outputs: Seq[OutputInfo],
  val params: Seq[ParamInfo]
);

object TaskInfo {
  // TODO: Additional detailed status
  // TODO: Running for how long? Start time? Finish time?
  val Status = Seq("blocked", "queued", "running", "done", "failed")
}

class WorkflowDatabase(dbFile: File) {
  Class.forName("org.sqlite.JDBC")
  private val conn = DriverManager.getConnection("jdbc:sqlite:" + dbFile.getAbsolutePath)
  conn.setAutoCommit(false)
  private val stat = conn.createStatement
  
  //stat.executeUpdate("create table if not exists tasks (task_name);")
  //stat.executeUpdate("create table if not exists dependencies (consequent, antecedent);")
  
  stat.executeUpdate("create table if not exists real_tasks (task_name, real_name, status);")
    // note: src_* fields may be *none*
  stat.executeUpdate("create table if not exists real_inputs (task_name, real_name, input_name, path, src_task, src_real);")
  stat.executeUpdate("create table if not exists real_outputs (task_name, real_name, output_name, output_path);")
  stat.executeUpdate("create table if not exists real_params (task_name, real_name, param_name, param_value, src_task, src_real);")

  // TODO: Globals?
  
  val taskInsert = conn.prepareStatement("insert into real_tasks values (?,?,?);")
  val inputInsert = conn.prepareStatement("insert into real_inputs values (?,?,?,?,?,?);")
  val outputInsert = conn.prepareStatement("insert into real_outputs values (?,?,?,?);")
  val paramInsert = conn.prepareStatement("insert into real_params values (?,?,?,?,?,?);")
  
  private def execute(stat: PreparedStatement, fillers: Seq[String]) {
    fillers.zipWithIndex.foreach { case (filler, i) => stat.setString(i+1, filler) }
    stat.execute() // wait until commit to save
  }
  
  def addTask(task: TaskInfo) {
    execute(taskInsert, Seq(task.name, task.realName, task.status))
    task.inputs.foreach { in =>
      execute(inputInsert, Seq(task.name, task.realName, in.name, in.path.getAbsolutePath,
          in.srcTaskAndReal.map(_._1).getOrElse(""),
          in.srcTaskAndReal.map(_._2).getOrElse("")))
    }
    task.outputs.foreach { out => execute(outputInsert, Seq(task.name, task.realName, out.name, out.path.getAbsolutePath)) }
    task.params.foreach { param =>
      execute(paramInsert, Seq(task.name, task.realName, param.name, param.value,
          param.srcTaskAndReal.map(_._1).getOrElse(""),
          param.srcTaskAndReal.map(_._2).getOrElse("")))
    }
  }
  
  private def query(stat: PreparedStatement, fillers: Seq[String], callback: ResultSet => Unit) {    
    fillers.zipWithIndex.foreach { case (filler, i) => stat.setString(i+1, filler) }
    val rs = stat.executeQuery()
    try {
      while (rs.next()) {
        callback(rs)
      }
    } finally {
      rs.close()
    }
  }

  val taskSelect = conn.prepareStatement("select * from real_tasks;")
  val inputSelect = conn.prepareStatement("select * from real_inputs where task_name=? and real_name=?;")
  val outputSelect = conn.prepareStatement("select * from real_outputs where task_name=? and real_name=?;")
  val paramSelect = conn.prepareStatement("select * from real_params where task_name=? and real_name=?;")

  // TODO: Synchronize on statements?
  def getTasks(): Seq[TaskInfo] = {
    val rs = taskSelect.executeQuery()
    val names = new mutable.ListBuffer[(String,String,String)]
    try {
      while (rs.next()) {
        names += ((rs.getString("task_name"), rs.getString("real_name"), rs.getString("status")))
      }
    } finally {
      rs.close()
    }
    
    val tasks: Seq[TaskInfo] = names.map { case (name, real, status) =>
      
      val packages = new mutable.ListBuffer[PackageInfo]
      // TODO: Packages
      
      val inputs = new mutable.ListBuffer[InputInfo]
      query(inputSelect, Seq(name, real), { rs =>
        val srcTask = rs.getString("src_task")
        val srcReal = rs.getString("src_real")
        val src = srcTask match {
          case "" => None
          case _ => Some( (srcTask, srcReal) )
        }
        inputs += new InputInfo(rs.getString("input_name"), new File(rs.getString("path")), src)
      })
      
      val outputs = new mutable.ListBuffer[OutputInfo]
      query(outputSelect, Seq(name, real), { rs =>
        outputs += new OutputInfo(rs.getString("input_name"), new File(rs.getString("path")))
      })
      
      val params = new mutable.ListBuffer[ParamInfo]
      query(paramSelect, Seq(name, real), { rs =>
        val srcTask = rs.getString("src_task")
        val srcReal = rs.getString("src_real")
        val src = srcTask match {
          case "" => None
          case _ => Some( (srcTask, srcReal) )
        }
        params += new ParamInfo(rs.getString("param_name"), rs.getString("param_value"), src)
      })
      
      new TaskInfo(name, real, status, packages, inputs, outputs, params)
    }
    tasks
  }
  
  def commit() = conn.commit()
  def close() = conn.close()
}

// represents the serialized workflow state from a DB
class WorkflowState {
  def toGraphViz(): String = {
    ""
  }
}
