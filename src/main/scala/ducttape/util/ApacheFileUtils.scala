/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ducttape.util

import java.io.File
import java.io.IOException

/**
 * Code translated into Scala from the original Java implementation
 * of org.apache.commons.io.FileUtils
 */
object ApacheFileUtils {

  /**
   * Deletes a directory recursively.
   *
   * @param directory  directory to delete
   * @throws IOException in case deletion is unsuccessful
   */
  def deleteDirectory(directory:File) {
      if (!directory.exists()) {
          return;
      }

      if (!isSymlink(directory)) {
          cleanDirectory(directory);
      }

      if (!directory.delete()) {
          val message:String =
              "Unable to delete directory " + directory + ".";
          throw new IOException(message);
      }
  }

  /**
   * Cleans a directory without deleting it.
   *
   * @param directory directory to clean
   * @throws IOException in case cleaning is unsuccessful
   */
  def cleanDirectory(directory:File) {
      if (!directory.exists()) {
          val message:String = directory + " does not exist";
          throw new IllegalArgumentException(message);
      }

      if (!directory.isDirectory()) {
          val message:String = directory + " is not a directory";
          throw new IllegalArgumentException(message);
      }

      val files = directory.listFiles();
      if (files == null) {  // null if security restricted
          throw new IOException("Failed to list contents of " + directory);
      }

      var exception:IOException = null;
      for (file <- files) {
          try {
              forceDelete(file);
          } catch {
            case (ioe:IOException) => {
              exception = ioe;
            }
          }
      }

      if (null != exception) {
          throw exception;
      }
  }

  /**
   * Deletes a file. If file is a directory, delete it and all sub-directories.
   * <p>
   * The difference between File.delete() and this method are:
   * <ul>
   * <li>A directory to be deleted does not have to be empty.</li>
   * <li>You get exceptions when a file or directory cannot be deleted.
   *      (java.io.File methods returns a boolean)</li>
   * </ul>
   *
   * @param file  file or directory to delete, must not be <code>null</code>
   * @throws NullPointerException if the directory is <code>null</code>
   * @throws FileNotFoundException if the file was not found
   * @throws IOException in case deletion is unsuccessful
   */
  def forceDelete(file:File) {
      if (file.isDirectory()) {
          deleteDirectory(file);
      } else {
          val filePresent = file.exists();
          if (!file.delete()) {
              if (!filePresent){
                  throw new java.io.FileNotFoundException("File does not exist: " + file);
              }
              val message:String =
                  "Unable to delete file: " + file;
              throw new IOException(message);
          }
      }
  }
  
  
  /**
   * Determines whether the specified file is a Symbolic Link rather than an actual file.
   * <p>
   * Will not return true if there is a Symbolic Link anywhere in the path,
   * only if the specific file is.
   * <p>
   * <b>Note:</b> the current implementation always returns {@code false} if the system
   * is detected as Windows using {@link FilenameUtils#isSystemWindows()}
   *
   * @param file the file to check
   * @return true if the file is a Symbolic Link
   * @throws IOException if an IO error occurs while checking the file
   * @since 2.0
   */
  def isSymlink(file:File):Boolean = {
      if (file == null) {
          throw new NullPointerException("File must not be null");
      }
      if (isSystemWindows) {
          return false;
      }
      var fileInCanonicalDir:File = null;
      if (file.getParent() == null) {
          fileInCanonicalDir = file;
      } else {
          val canonicalDir:File = file.getParentFile().getCanonicalFile();
          fileInCanonicalDir = new File(canonicalDir, file.getName());
      }

      if (fileInCanonicalDir.getCanonicalFile().equals(fileInCanonicalDir.getAbsoluteFile())) {
          return false;
      } else {
          return true;
      }
  }


  /**
   * The Windows separator character.
   */
  val WINDOWS_SEPARATOR = '\\';

  /**
   * The system separator character.
   */
  val SYSTEM_SEPARATOR = File.separatorChar;  
  
  /**
   * Determines if Windows file system is in use.
   *
   * @return true if the system is Windows
   */
  def isSystemWindows:Boolean = {
      SYSTEM_SEPARATOR == WINDOWS_SEPARATOR;
  }

  
}