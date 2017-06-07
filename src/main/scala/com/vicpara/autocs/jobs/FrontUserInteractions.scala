package com.vicpara.autocs.jobs

import java.io.{ File, FileFilter, FilenameFilter }
import java.nio.file.Paths
import java.util.UUID
import java.util.concurrent.Executors

import com.fasterxml.jackson.annotation.ObjectIdGenerators.UUIDGenerator
import com.vicpara.autocs.app.{ AppConfig, AppLogger, Utils }
import com.vicpara.autocs.io._
import com.vicpara.autocs.learning.AnswerOracle

import scala.language.reflectiveCalls
import org.rogach.scallop.ScallopConf

import scala.concurrent.{ Await, ExecutionContext }
import scala.concurrent.duration._

//import scala.concurrent.ExecutionContext.Implicits.global
import scalax.file.Path

// java -cp $(find /Users/victor/work/auto-cs -name "*.jar" | tr "\n" ":") com.vicpara.autocs.jobs.FrontUserInteractions
object FrontUserInteractions {

  val ioThreadPool = Executors.newWorkStealingPool(4)
  implicit val ec = new ExecutionContext {
    val threadPool = ioThreadPool

    def execute(runnable: Runnable) {
      threadPool.submit(runnable)
    }

    def reportFailure(t: Throwable): Unit = {
    }
  }

  def main(args: Array[String]) {
    val conf = new ScallopConf(args) {
      val key = opt[String](required = false, descr = "Key to use to retrieve messages")
      val rw = opt[Boolean](required = false, descr = "Run just ReadWrite test")
    }
    conf.printHelp()
    Console.print(conf.summary)

  }
}
