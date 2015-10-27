import java.io.File
import java.util.concurrent.{ConcurrentHashMap, LinkedBlockingDeque}
import java.util.concurrent.atomic.AtomicReference

import myactors.utils
import org.apache.commons.io.FileUtils

import scala.annotation.tailrec
import scala.collection._
import scala.collection.concurrent.TrieMap
import scala.collection.convert.decorateAsScala._
import scala.concurrent.ExecutionContext
import scala.sys.process._


/**
 * A file system class that demonstrates the use of the concurrent versions
 * of common data structures in scala. The files itself is a type of data structure itself
 * @param root
 *      The root directory of the file system.
 */
class FileSystem(val root: String) {
  //The logger thread logging messages to the console as they are created by the
  //file system.
  val logger = new Thread {
    setDaemon(true)
    override def run() = {
      while (true) {
        val msg = messages.take()
        log(msg)
      }
    }
  }
  //A ConcurrentHashMap : thread safe hash map
  val files: concurrent.Map[String, Entry] = new concurrent.TrieMap
  logger.start()
  //Variable containing the messages for logging.
  private val messages = new LinkedBlockingDeque[String]()

  /**
   * Adds a message to the messages log.
   * @param msg
   *      Message to log.
   */
  def logMessage(msg: String): Unit = messages.add(msg)

  /**
   * Matches over the file's isDir property to check whether
   * the file is a directory or not. If it is not then
   * an attempt to prepare the file for deletion is made. If the prepareForDelete
   * method returns true, this signals that prepareForDelete was able to cas
   * the file's state into Deleting and the file can be safely deleted.
   * The block to delete the file is passed by name to the execute method
   * to asynchronously delete the file, since we do not want to block the
   * caller thread.
   * @param filename
   *                 The name of the file to be deleted.
   */
  def deleteFile(filename: String): Unit = {
    files.get(filename) match {
      case None =>
        logMessage(s"Path '$filename' does not exist!")
      case Some(entry) if entry.isDir =>
        logMessage(s"Path '$filename' is a directory!")
      case Some(entry) => execute {
        if (prepareForDelete(entry)) {
          if (FileUtils.deleteQuietly(new File(filename))) {
            files.remove(filename)
          }
        }
      }
    }
  }

  /**
   * The copy file processing method proceeds only if the file is not a directory
   * and exits. If the file exists a block executing a copy is executed in a thread.
   *
   * @param src
   *              The name of the file being c  process.
opied.
   * @param dest
   *             The destination for the file.
   */
  def copyFile(src: String, dest: String): Unit = {
    files.get(src) match {
      case None => logMessage(s"File '$src' does not exits.")
      case Some(srcEntry) if srcEntry.isDir =>
        sys.error(s"Path '$src' is a directory!")
      case Some(srcEntry) => execute {
        if (acquire(srcEntry)) try {
          val destEntry = new Entry(false)
          destEntry.state.set(new Creating)
          //@putIfAbsent returns None if the map did not contain
          //a previous value or Some(entry) if their was one.
          //Here we continue if there was not a previous entry
          if (files.putIfAbsent(dest, destEntry).isEmpty) try {
            FileUtils.copyFile(new File(src), new File(dest))
            //Always release the lock the operation passes or fails
            //to avoid dead locks and starvation in the system.
          } finally release(destEntry)
        } finally release(srcEntry)
      }
    }
  }

  /**
   *
   * Prints the current thread's name and the message to log.
   *
   * @param s
   *      The string to log.
   */
  def log(s: String): Unit = {
    println(Thread.currentThread().getName + " " + s)
  }

  /**
   * Sends a body to be executed to the global executor.
   * @param body
   * The body to be executed.
   */
  def execute(body: => Unit) = ExecutionContext.global.execute(
    new Runnable {
      def run() = body
    })

  /**
   * State altering method for CASing the state of a file to Deleting.
   * @param entry
   * An instance of a file entry being prepared for deletion.
   * @return
   * True if state of entry is cas'd to a deletion state. If entry in state
   * of copying, creating, or deleting false is return and user/logger is notified
   * of the current state of the file.
   */
  @tailrec private def prepareForDelete(entry: Entry): Boolean = {
    val s0 = entry.state.get
    s0 match {
      //A failed CAS warrants a recursive recursive call on the CAS method.
      //Of course when doing this the method must have case/if's for the conditions that do not
      //lead to an infinite reCAS. ie. CAS base cases.
      case i: Idle =>
        if (entry.state.compareAndSet(s0, new Deleting)) true
        else prepareForDelete(entry)
      case c: Copying =>
        logMessage("File currently being copied, cannot delete.")
        false
      case c: Creating =>
        logMessage("File currently being created, cannot delete.")
        false
      case d: Deleting =>
        false
    }
  }

  /**
   *
   * Attempts to acquire the file by CAS'ing the state of the file
   * entry to copying, or increasing the copying counter on some file
   * that is already being copied.
   *
   * @param entry
   * A file entry.
   * @return
   * True if the file was acquired, false if not.
   */
  @tailrec private def acquire(entry: Entry): Boolean = {
    val s0 = entry.state.get
    s0 match {
      case _: Creating | _: Deleting =>
        logMessage("File inaccessible, cannot copy.")
        false
      case i: Idle =>
        if (entry.state.compareAndSet(s0, new Copying(1))) true
        else acquire(entry)
      case c: Copying =>
        if (entry.state.compareAndSet(s0, new Copying(c.n + 1))) true
        else acquire(entry)
    }
  }

  /*
   * The asScala casts a Java collection to a Scala collection.
   * In the below case the iterator return by FileUtils.iterateFiles method
   * is casted into a Scala collection implementing the Scala collection API
   * so we can for comprehend on it.
   * No blocking is needed as the TrieMap's put is atomic.
   */
  for (f <- FileUtils.iterateFiles(new File(root), null, false).asScala) {
    //Iterates through all files in the Project directory and places them in a TrieMap
    //Keys : Name of file, Value : File Entry instance -> contains state and type of file.
    files.put(f.getName, new Entry(false))
  }

  /**
   * Method to release a file after a process is done with it.
   * @param entry
   * A file entry being released by some process.
   */
  @tailrec private def release(entry: Entry): Unit = {
    val s0 = entry.state.get
    s0 match {
      case i: Idle =>
        sys.error("Error - released more times than acquired.")
      case c: Creating =>
        if (!entry.state.compareAndSet(s0, new Idle)) release(entry)
      case c: Copying if c.n <= 0 =>
        sys.error("Error - cannot have 0 or less copies in progress!")
      case c: Copying =>
        val newState = if (c.n == 1) new Idle else new Copying(c.n - 1)
        if (!entry.state.compareAndSet(s0, newState)) release(entry)
      case d: Deleting =>
        sys.error("Error - releasing a file that is being deleted!")
    }
  }

  /**  process.

   * Trait for defining a files state.
   */
  sealed trait State

  /*
   *
   * Acquisition, process, and release methods for File copying
   *
   */

  /**
   * Class defining an Idle file state.
   */
  class Idle extends State

  /**
   * Class defining a Creating file state.
   */
  class Creating extends State

  /**
   * Class defining an in Copying state for a file. Information exists in this class that
   * represents
   * the number of copy requests that remain for this file.
   * @param n
   * The number of copy requests for this file.
   */
  class Copying(val n: Int) extends State

  /**
   * Class for defining a state for a file in Deletion.
   */
  class Deleting extends State

  /**
   * Class representing an Entry in the file system. The state is set to an Idle instance by default in
   * an @atomic reference.
   *
   * @param isDir
   * Represents the type of file. True if the file is a directory.
   * False if it is a file.
   */
  class Entry(val isDir: Boolean) {
    val state = new AtomicReference[State](new Idle)
  }
}
object ConcurrentUtils {

  /**
   * Sends a body to be executed to the global executor.
   * @param body
   * The boyd to be executed.
   */
  def execute(body: => Unit) = ExecutionContext.global.execute(
    new Runnable {
      def run() = body
    })

  /**
   *
   * Prints the current thread's name and the message to log.
   * @param s
   *      The string to log.
   */
  def log(s: String): Unit = {
    println("Outputting " + " " + s)
  }

//  val logger = new Thread {
//    setDaemon(true)
//    override def run() = {
//      while (true) {
//        val msg = messages.take()
//        log(msg)
//      }
//    }
//  }
}

object FileSystem extends App {
  val fileSystem = new FileSystem(".")
  fileSystem.log("Testing logger!")
  fileSystem.deleteFile("test.txt")
  fileSystem.files.foreach((entry) => {
    println(entry._1 + " " + entry._2.getClass.getSimpleName)
  })

  def concurrentIterators(): Unit = {
    //A concurrent hash map does not create a consistent iterator
    val names = new ConcurrentHashMap[String, Int]().asScala
    names("Johnny") = 0;
    names("Jane") = 0;
    names("Jack") = 0
    utils.execute {
      for (n <- 0 until 10) names(s"John $n") = n
    }
    utils.execute {
      for (n <- names) utils.log(s"name: $n")
    }
    Thread.sleep(1000)

    //A trie map does
    val trieMap = new ConcurrentHashMap[String, Int]()
    val names2 = new ConcurrentHashMap[String, Int]().asScala
    names2("Janice") = 0;
    names2("Jackie") = 0;
    names2("Jill") = 0
    utils.execute {
      for (n <- 10 until 100) names2(s"John $n") = n
    }
    utils.execute {
      utils.log("snapshot time!")
      for (n <- names2.keys.toSeq.sorted) utils.log(s"name : $n")
    }
    Thread.sleep(1000)
  }

  def linCount(filename : String) : Int = {
    val output = s"wc $filename".!!
    output.trim.split(" ").head.toInt
  }
  //With the
  val command = "ls"
  val exitCode = command !
  val printedCode = exitCode.toString
  utils.log(s"command exited with status: $printedCode")

  val lsProcess = "ls -R /".run()
  Thread.sleep(1000)
  utils.log("Timeout - killing ls!")
  lsProcess.destroy()
}