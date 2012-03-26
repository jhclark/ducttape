package ducttape.db

import java.sql._
import collection._

class WorkflowDatabase(dbFile: String) {
  Class.forName("org.sqlite.JDBC")
  private val conn = DriverManager.getConnection("jdbc:sqlite:" + dbFile)
  conn.setAutoCommit(false)
  private val stat = conn.createStatement
  stat.executeUpdate("create table if not exists tasks (task_name);")
  stat.executeUpdate("create table if not exists dependencies (consequent, antecedent);")
  stat.executeUpdate("create table if not exists real_tasks (task_name, real_name);")

  val taskStat = conn.prepareStatement("insert into tasks values (?);")
  def addTask(name: String) {
    taskStat.setString(1, name)
    taskStat.execute // wait until commit to save
  }

  def getTasks(): Seq[String] = {
    val rs = stat.executeQuery("select * from tasks;")
    val result = new mutable.ListBuffer[String]
    while(rs.next) {
      result += rs.getString("task_name")
    }
    rs.close
    result
  }
  
  def commit() = conn.commit
  def close() = conn.close

  addTask("tok")
  addTask("extract_gra_dev")
  addTask("extract_gra_test")
  addTask("tune")
  addTask("decode")
  commit
}

// represents the serialized workflow state from a DB
class WorkflowState {
  def toGraphViz(): String = {
    ""
  }
}
