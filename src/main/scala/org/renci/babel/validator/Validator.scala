package org.renci.babel.validator

import java.io.File
import com.typesafe.scalalogging._
import org.rogach.scallop._
import zio._
import org.renci.babel.validator.model.BabelOutput

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._

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

  def filterFilename(filename: String): Boolean = {
    if (!filteredIn.isEmpty) {
      if (filteredIn.exists(filename.startsWith(_))) {
        return true;
      } else {
        return false;
      }
    }

    if (!filteredOut.isEmpty && !filteredOut.exists(filename.startsWith(_))) {
      return false;
    }

    true
  }

  val runtime = Runtime.default

  println(s"Profile of Babel output ${babelOutput}")
  println(babelOutput)

  val countsByFilename = mutable.HashMap[String, Long]()
  val summaries = babelOutput.compendiaSummary
  for {
    summary <- summaries
    filename = summary.filename
    to_filter = filterFilename(filename)
  } yield {
    if (to_filter) {
      val count = runtime.unsafeRun(summary.countZIO)
      countsByFilename.put(filename, count)
      println(s"Number of lines in compendium ${summary.filename}: ${count}")
    } else {
      println(s"Skipping compendium ${summary.filename}")
    }
  }

  def relativePercentChange(count: Long, countPrev: Long): String = {
    val percentChange = (count-countPrev).toDouble/countPrev * 100
    f"${count - countPrev}%+d\t$percentChange%+2.4f%%"
  }

  val summariesMap = summaries.groupBy(_.filename)
  val summariesPrev = babelPrevOutputOpt.get.compendiaSummary

  // TODO: this should actually loop over the current filename counts, so we can flag where the prev run is missing
  // files for the new one.
  println(s"# Comparison with ${babelPrevOutputOpt.map(_.compendiaDir)}")
  for {
    summaryPrev <- summariesPrev
    filename = summaryPrev.filename
    to_filter = filterFilename(filename)
  } yield {
    if (to_filter) {
      val count = countsByFilename.getOrElse[Long](filename, 0)
      val compareToCount = runtime.unsafeRun(summaryPrev.countZIO)
      println(s"$filename\t$count\t$compareToCount\t${relativePercentChange(count, compareToCount)}")
    } else {
      println(s"Skipping compendium ${filename}")
    }
  }

  // TODO:
  // - Add processing time, preferably broken down by compendium or something (maybe just emit logs?)
  // - Some stats on memory usage would be great too
}