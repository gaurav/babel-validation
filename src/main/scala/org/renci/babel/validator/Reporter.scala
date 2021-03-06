package org.renci.babel.validator

import com.typesafe.scalalogging.LazyLogging
import org.renci.babel.validator.Validator.Conf
import org.renci.babel.validator.model.{BabelOutput, Compendium}
import zio.ZIO
import zio.blocking.Blocking
import zio.console.Console
import zio.stream.ZStream

import java.io.{FileOutputStream, PrintStream}

/** Functions for reporting on the differences between two input files.
  */
object Reporter extends LazyLogging {

  /** Helper method for displaying the percent change between two counts.
    */
  def relativePercentChange(count: Long, countPrev: Long): String = {
    val percentChange = (count - countPrev).toDouble / countPrev * 100
    f"${count - countPrev}%+d\t$percentChange%+2.2f%%"
  }

  /** Generic method to determine whether a particular filename should be
    * filtered in or out from the results. The algorithm we use is:
    *   1. If any `--filtered-in` prefixes are provided, then we exclude
    *      everything that isn't explicitly filtered in (by starting with one of
    *      those prefixes in a case-sensitive manner). 2. Otherwise, all
    *      filenames are allowed EXCEPT those explicitly filtered out by
    *      `--filtered-out` by starting with one of those prefixes in a
    *      case-sensitive manner.
    */
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

  /** Given two BabelOutputs, it returns a list of all compendia found in BOTH
    * of the BabelOutputs paired together.
    *
    * TODO: modify this so we return every compendium found in EITHER
    * BabelOutput.
    */
  def retrievePairedCompendiaSummaries(
      babelOutput: BabelOutput,
      babelPrevOutput: BabelOutput
  ): Seq[(String, Compendium, Compendium)] = {
    for {
      summary <- babelOutput.compendia
      summaryPrev <- babelPrevOutput.compendia
      if summaryPrev.filename == summary.filename
    } yield {
      (summary.filename, summary, summaryPrev)
    }
  }

  def diffResults(conf: Conf): ZIO[Blocking with Console, Throwable, Unit] = {
    val babelOutput = new BabelOutput(conf.babelOutput())
    val babelPrevOutput = new BabelOutput(conf.babelPrevOutput())
    val output = conf.output.toOption match {
      case Some(file) => new PrintStream(new FileOutputStream(file))
      case _          => System.out
    }

    val pairedSummaries =
      retrievePairedCompendiaSummaries(babelOutput, babelPrevOutput)
    output.println("Filename\tCount\tPrevCount\tDiff\tPercentageChange")
    ZStream
      .fromIterable(pairedSummaries)
      .mapMParUnordered(conf.nCores()) {
        case (
              filename: String,
              summary: Compendium,
              prevSummary: Compendium
            ) if filterFilename(conf, filename) => {

          for {
            lengthComparison <- Comparer.compareLengths(
              filename,
              summary,
              prevSummary
            )
            typeComparison <- Comparer.compareTypes(
              filename,
              summary,
              prevSummary
            )
          } yield {
            output.println(lengthComparison.toString)
            output.println(typeComparison.toString)
          }
        }
        case (filename: String, _, _) if !filterFilename(conf, filename) => {
          logger.info(s"Skipping ${filename}")
          ZIO.succeed(())
        }
        case abc =>
          ZIO.fail(new RuntimeException(s"Invalid paired summary: ${abc}"))
      }
      .runDrain
  }
}
