package org.renci.babel.validator

import java.io.{File, IOException}
import com.typesafe.scalalogging._
import org.rogach.scallop._
import zio._
import org.renci.babel.validator.model.{BabelOutput, Compendium}
import zio.blocking.Blocking

import scala.collection.mutable
import zio.console._
import zio.stream.ZStream

import java.util.Date

object Validator extends zio.App with LazyLogging {
  class Conf(args: Seq[String]) extends ScallopConf(args) {
    val babelOutput = trailArg[File](descr = "The current Babel output directory", required = true)
    val babelPrevOutput = trailArg[File](descr = "The previous Babel output", required = true)
    validateFileIsDirectory(babelOutput)
    validateFileIsDirectory(babelPrevOutput)

    val filterIn = opt[List[String]](descr = "List of filenames to include (matched using startsWith)")
    val filterOut = opt[List[String]](descr = "List of filenames to exclude (matched using startsWith)")

    val nCores = opt[Int](descr = "Number of cores to use")

    verify()
  }

  def filterFilename(conf: Conf, filename: String): Boolean = {
    val filteredIn = conf.filterIn.getOrElse(List())
    val filteredOut = conf.filterOut.getOrElse(List())

    if (filteredIn.nonEmpty) {
      if (filteredIn.exists(filename.startsWith(_))) {
        return true;
      } else {
        return false;
      }
    }

    if (filteredOut.nonEmpty && filteredOut.exists(filename.startsWith(_))) {
      return false;
    }

    true
  }

  def retrievePairedCompendiaSummaries(babelOutput: BabelOutput, babelPrevOutput: BabelOutput): Seq[(String, Compendium#Summary, Compendium#Summary)] = {
    for {
      summary <- babelOutput.compendiaSummary
      summaryPrev <- babelPrevOutput.compendiaSummary if summaryPrev.filename == summary.filename
    } yield {
      (summary.filename, summary, summaryPrev)
    }
  }

  def relativePercentChange(count: Long, countPrev: Long): String = {
    val percentChange = (count - countPrev).toDouble / countPrev * 100
    f"${count - countPrev}%+d\t$percentChange%+2.4f%%"
  }

  def diffResults(conf: Conf): ZIO[Blocking with Console, Throwable, Unit] = {
    val babelOutput = new BabelOutput(conf.babelOutput())
    val babelPrevOutput = new BabelOutput(conf.babelPrevOutput())

    val pairedSummaries = retrievePairedCompendiaSummaries(babelOutput, babelPrevOutput)
    println("Filename\tCount\tPrevCount\tDiff\tPercentageChange")
    val startTime = System.nanoTime()
    ZStream.fromIterable(pairedSummaries)
      .mapMParUnordered(conf.nCores())({
        case (filename: String, summary: Compendium#Summary, prevSummary: Compendium#Summary) if filterFilename(conf, filename) => {
          for {
            count <- summary.countZIO
            prevCount <- prevSummary.countZIO
            _ = putStrLn(s"putStrLn: ${filename}\t${count}\t${prevCount}\t${relativePercentChange(count, prevCount)}")
          } yield {
            println(s"${filename}\t${count}\t${prevCount}\t${relativePercentChange(count, prevCount)}")
          }
        }
        case (filename: String, _, _) if !filterFilename(conf, filename) => {
          logger.info(s"Skipping ${filename}")
          ZIO.succeed()
        }
        case abc => ZIO.fail(new RuntimeException(s"Invalid paired summary: ${abc}"))
      })
      .runDrain
      .andThen({
        val timeTaken_secs = (System.nanoTime() - startTime).toDouble / 1E9
        logger.info(f"Calculated counts on ${conf.nCores()} cores in $timeTaken_secs%.4f seconds")
        ZIO.succeed()
      })
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