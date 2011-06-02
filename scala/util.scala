package ducttape.util

object Files {
  import java.io._
  def write(str: String, file: File) = {
    val fw = new FileWriter(file)
    fw.write(str)
    fw.close()    
  }
}
