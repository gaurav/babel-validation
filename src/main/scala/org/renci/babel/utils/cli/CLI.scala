package org.renci.babel.utils.cli

import com.typesafe.scalalogging.{LazyLogging, Seq}
import org.renci.babel.utils.converter.Converter
import org.rogach.scallop.{ScallopConf, ScallopConfBase, ScallopOption}
import zio.blocking.Blocking
import zio.console.Console
import zio.{ExitCode, URIO, ZIO}

/**
 * The entrypoint for Babel Utils. The subcommand name controls which of the
 * utils will be invoked. The following subcommands are currently supported:
 *   - diff (DiffReporter)
 *   - convert (Converter)
 *
 * This is architected so that each of those classes is responsible for what is
 * supported by that action, and this object only stores shared code.
 */
object CLI extends zio.App with LazyLogging {

  /**
   * The Scallop configuration for this entire application, which consists of a
   * series of Subcommands provided by the individual classes.
   *
   * @param args
   *   The command-line arguments to this application.
   */
  class Conf(args: Seq[String]) extends ScallopConf(args) {
    addSubcommand(new DiffReporter.DiffSubcommand())
    addSubcommand(new Converter.ConvertSubcommand())
    requireSubcommand()
    verify()
  }

  /**
   * Entrypoint to this application.
   *
   * TODO:
   *   - Some stats on overall memory usage would be great too
   *
   * @param args
   *   Command line arguments.
   */
  def run(
      args: List[String]
  ): URIO[Blocking with Console with Console, ExitCode] = {
    val conf = new Conf(args)
    val zioURIO = conf.subcommand match {
      case Some(diff: DiffReporter.DiffSubcommand) =>
        DiffReporter.diffResults(diff).exitCode
      case Some(convert: Converter.ConvertSubcommand) =>
        Converter.convert(convert).exitCode
      case a =>
        ZIO.fail(s"Error: no subcommand provided or invalid (${a})").exitCode
    }

    val startTime = System.nanoTime()
    zioURIO map (exitCode => {
      val endTime = System.nanoTime()
      val timeTakenInSeconds = (endTime - startTime).toDouble / 1_000_000_000
      logger.info(f"Completed in ${timeTakenInSeconds}%.2f seconds")
      exitCode
    })
  }

  /**
   * Helper method for displaying the percent change between two counts.
   */
  def relativePercentChange(count: Long, countPrev: Long): String = {
    val percentChange = (count - countPrev).toDouble / countPrev * 100
    f"${count - countPrev}%+d\t$percentChange%+2.2f%%"
  }

  /**
   * Generic method to determine whether a particular filename should be
   * filtered in or out from the results. The algorithm we use is:
   *
   *   1. If any `--filtered-in` prefixes are provided, then we exclude
   *      everything that isn't explicitly filtered in (by starting with one of
   *      those prefixes in a case-sensitive manner).
   *
   * 2. Otherwise, all filenames are allowed EXCEPT those explicitly filtered
   * out by `--filtered-out` by starting with one of those prefixes in a
   * case-sensitive manner.
   */
  def filterFilename(
      conf: SupportsFilenameFiltering,
      filename: String
  ): Boolean = {
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

  /**
   * filterFilename() can work with any Scallop configuration that includes `--filter-in`
   * and `--filter-out` options. We can include those fields in a consistent way by including
   * this trait in a configuration.
   */
  trait SupportsFilenameFiltering extends ScallopConfBase {
    val filterIn: ScallopOption[List[String]] = opt[List[String]](descr =
      "List of filenames to include (matched using startsWith)"
    )
    val filterOut: ScallopOption[List[String]] = opt[List[String]](descr =
      "List of filenames to exclude (matched using startsWith)"
    )
  }
}
