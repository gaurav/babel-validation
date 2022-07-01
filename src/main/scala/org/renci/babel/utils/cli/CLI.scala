package org.renci.babel.utils.cli

import com.typesafe.scalalogging.{LazyLogging, Seq}
import org.renci.babel.utils.converter.Converter
import org.rogach.scallop.ScallopConf
import zio.blocking.Blocking
import zio.console.Console
import zio.{ExitCode, URIO, ZIO}

/**
 * The entrypoint for Babel Utils. The subcommand name controls which of the
 * utils will be invoked. The following subcommands are currently supported:
 *  - diff (DiffReporter)
 *  - convert (Converter)
 *
 * This is architected so that each of those classes is responsible for what is
 * supported by that action, and this object only stores shared code.
 */
object CLI extends zio.App with LazyLogging {

  /**
   * The Scallop configuration for this entire application, which consists of a series of
   * Subcommands provided by the individual classes.
   *
   * @param args The command-line arguments to this application.
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
   *  - Some stats on overall memory usage would be great too
   *
   * @param args Command line arguments.
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
      val timeTakenInSeconds = (endTime - startTime).toDouble/1_000_000_000
      logger.info(f"Completed in ${timeTakenInSeconds}%.2f seconds")
      exitCode
    })
  }
}
