package ducttape.util

import java.util.logging.Formatter
import java.util.logging.LogRecord

import collection._
import collection.JavaConversions._
import java.lang.management.ManagementFactory

class LogFormatter extends Formatter {
    
    val startTime = ManagementFactory.getRuntimeMXBean.getStartTime
    
    override def format(record: LogRecord): String = formatMessage(record) + "\n"
    
    override def formatMessage(record: LogRecord): String = {
     // NOTE: Thread.currentThread.getId != record.getThreadID   
      "[%d] %s: %s [%s]: %s".format(
          record.getMillis - startTime,
          record.getLevel,
          record.getLoggerName,
          Thread.currentThread.getName,
          record.getMessage)
    }
}