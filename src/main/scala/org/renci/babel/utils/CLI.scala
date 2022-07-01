package org.renci.babel.utils

import com.typesafe.scalalogging._
import org.renci.babel.utils.converter.Converter
import org.rogach.scallop._
import zio._
import zio.blocking.Blocking
import zio.console._

object CLI extends zio.App with LazyLogging {
  class Conf(args: Seq[String]) extends ScallopConf(args) {
    addSubcommand(new DiffReporter.DiffSubcommand())
    addSubcommand(new Converter.ConvertSubcommand())
    requireSubcommand()
    verify()
  }

  // TODO:
  // - Add processing time, preferably broken down by compendium or something (maybe just emit logs?)
  // - Some stats on memory usage would be great too

  /** Entrypoint.
    *
    * @param args
    *   Command line arguments.
    */
  def run(
      args: List[String]
  ): URIO[Blocking with Console with Console, ExitCode] = {
    val conf = new Conf(args)
    conf.subcommand match {
      case Some(diff: DiffReporter.DiffSubcommand) =>
        DiffReporter.diffResults(diff).exitCode
      case Some(convert: Converter.ConvertSubcommand) =>
        Converter.convert(convert).exitCode
      case a =>
        ZIO.fail(s"Error: no subcommand provided or invalid (${a})").exitCode
    }
  }
}
