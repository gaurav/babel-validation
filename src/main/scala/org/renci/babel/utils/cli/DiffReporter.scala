package org.renci.babel.utils.cli

import com.typesafe.scalalogging.LazyLogging
import org.renci.babel.utils.model.{BabelOutput, Compendium}
import org.rogach.scallop.{ScallopOption, Subcommand}
import zio.ZIO
import zio.blocking.Blocking
import zio.console.Console
import zio.stream.ZStream

import java.io.{File, FileOutputStream, PrintStream}

/**
 * Functions for reporting on the differences between two input files.
 */
object DiffReporter extends LazyLogging {

  /** The subcommand that controlling comparing. */
  class DiffSubcommand
      extends Subcommand("diff")
      with org.renci.babel.utils.cli.CLI.SupportsFilenameFiltering {
    val babelOutput: ScallopOption[File] = trailArg[File](
      descr = "The current Babel output directory",
      required = true
    )
    val babelPrevOutput: ScallopOption[File] =
      trailArg[File](descr = "The previous Babel output", required = true)
    validateFileIsDirectory(babelOutput)
    validateFileIsDirectory(babelPrevOutput)

    val nCores: ScallopOption[Int] = opt[Int](descr = "Number of cores to use")

    val output: ScallopOption[File] = opt[File](descr = "Output file")
  }

  /**
   * Given two BabelOutputs, it returns a list of all compendia found in BOTH of
   * the BabelOutputs paired together.
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

  def diffResults(
      conf: DiffSubcommand
  ): ZIO[Blocking with Console, Throwable, Unit] = {
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
            ) if CLI.filterFilename(conf, filename) => {

          for {
            // lengthComparison <- Comparer.compareLengths(filename, summary, prevSummary)
            // typeComparison <- Comparer.compareTypes(filename, summary, prevSummary)
            clusterComparison <- Comparer.compareClusters(
              filename,
              summary,
              prevSummary
            )
          } yield {
            // output.println(lengthComparison.toString)
            // output.println(typeComparison.toString)
            output.println(clusterComparison.toString)
          }
        }
        case (filename: String, _, _)
            if !CLI.filterFilename(conf, filename) => {
          logger.info(s"Skipping ${filename}")
          ZIO.succeed(())
        }
        case abc =>
          ZIO.fail(new RuntimeException(s"Invalid paired summary: ${abc}"))
      }
      .runDrain
  }
}
