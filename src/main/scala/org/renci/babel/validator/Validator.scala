package org.renci.babel.validator

import java.io.File
import com.typesafe.scalalogging._
import org.rogach.scallop._
import zio._
import org.renci.babel.validator.model.BabelOutput

object Validator extends scala.App with LazyLogging {
  class Conf(args: Seq[String]) extends ScallopConf(args) {
    val babelOutput = trailArg[File](descr = "The current Babel output directory", required = true)
    val babelPrevOutput = trailArg[File](descr = "The previous Babel output", required = false)
    validateFileIsDirectory(babelOutput)
    validateFileIsDirectory(babelPrevOutput)

    val filterIn = opt[List[String]](descr = "List of filenames to include (matched using startsWith)")
    val filterOut = opt[List[String]](descr = "List of filenames to exclude (matched using startsWith)")

    verify()
  }

  val conf = new Conf(args)
  val babelOutput = new BabelOutput(conf.babelOutput())
  val babelPrevOutputOpt: Option[BabelOutput] = conf.babelPrevOutput.toOption.map(new BabelOutput(_))

  val filteredIn = conf.filterIn.getOrElse(List())
  val filteredOut = conf.filterOut.getOrElse(List())

  val runtime = Runtime.default

  println(s"Profile of Babel output ${babelOutput}")
  println(babelOutput)

  for {
    summary <- babelOutput.compendiaSummary
    to_filter = (filteredIn.exists(filteredIn.isEmpty || summary.filename.startsWith(_)) ||
        (!filteredOut.exists(!filteredOut.isEmpty && summary.filename.startsWith(_))))
  } yield {
    if (to_filter) {
      val count = runtime.unsafeRun(summary.countZIO)
      println(s"Number of lines in compendium ${summary.filename}: ${count}")
    } else {
      println(s"Skipping compendium ${summary.filename}")
    }
  }

  if (!babelPrevOutputOpt.isEmpty) {
    val babelPrevOutput = babelPrevOutputOpt.get

    println(s"Profile of previous Babel output ${babelPrevOutput}")
    println(babelPrevOutput)

    println(s"Comparison of ${babelOutput} with previous ${babelPrevOutput}")
  }
}