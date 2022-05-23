package org.renci.babel.validator

import java.io.{File, IOException}
import com.typesafe.scalalogging._
import org.rogach.scallop._
import zio._
import org.renci.babel.validator.model.{BabelOutput, Compendium}

import scala.collection.mutable
import zio.console._

object Validator extends zio.App with LazyLogging {
  class Conf(args: Seq[String]) extends ScallopConf(args) {
    val babelOutput = trailArg[File](descr = "The current Babel output directory", required = true)
    val babelPrevOutput = trailArg[File](descr = "The previous Babel output", required = true)
    validateFileIsDirectory(babelOutput)
    validateFileIsDirectory(babelPrevOutput)

    val filterIn = opt[List[String]](descr = "List of filenames to include (matched using startsWith)")
    val filterOut = opt[List[String]](descr = "List of filenames to exclude (matched using startsWith)")

    verify()
  }

  def retrievePairedCompendiaSummaries(babelOutput: BabelOutput, babelPrevOutput: BabelOutput): Seq[(String, Compendium#Summary, Compendium#Summary)] = {
    for {
      summary <- babelOutput.compendiaSummary
      summaryPrev <- babelPrevOutput.compendiaSummary if summaryPrev.filename == summary.filename
    } yield {
      (summary.filename, summary, summaryPrev)
    }
  }

  def diffResults(conf: Conf): ZIO[Console, IOException, Unit] = {
    val babelOutput = new BabelOutput(conf.babelOutput())
    val babelPrevOutput = new BabelOutput(conf.babelPrevOutput())

    val pairedSummaries = retrievePairedCompendiaSummaries(babelOutput, babelPrevOutput)
    ZIO.foreach(pairedSummaries)((abc: (String, Compendium#Summary, Compendium#Summary)) => putStrLn(s" - ${abc._1}"))
      .andThen(ZIO.succeed())
  }

  def tbd(args: List[String]) = {
    val conf = new Conf(args)
    val babelOutput = new BabelOutput(conf.babelOutput())
    val babelPrevOutputOpt = new BabelOutput(conf.babelPrevOutput())

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
    val summaries = babelOutput.compendiaSummary
    val countsByFilename = mutable.HashMap[String, Long]()


    //

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
      val percentChange = (count - countPrev).toDouble / countPrev * 100
      f"${count - countPrev}%+d\t$percentChange%+2.4f%%"
    }

    val summariesMap = summaries.groupBy(_.filename)
    val summariesPrev = babelPrevOutputOpt.compendiaSummary

    // TODO: this should actually loop over the current filename counts, so we can flag where the prev run is missing
    // files for the new one.
    println(s"# Comparison with ${babelPrevOutputOpt.compendiaDir}")
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
  }

  // TODO:
  // - Add processing time, preferably broken down by compendium or something (maybe just emit logs?)
  // - Some stats on memory usage would be great too

  /**
   * Entrypoint.
   *
   * @param args Command line arguments.
   */
  def run(args: List[String]) = {
    diffResults(new Conf(args)).exitCode
  }
}