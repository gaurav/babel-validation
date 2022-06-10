package org.renci.babel.validator

import com.typesafe.scalalogging._
import org.rogach.scallop._
import zio._
import zio.blocking.Blocking
import zio.console._

import java.io.File

object Validator extends zio.App with LazyLogging {
  class ValidateSubcommand extends Subcommand("validate") {
    val babelOutput: ScallopOption[File] = trailArg[File](
      descr = "The current Babel output directory",
      required = true
    )
    val babelPrevOutput: ScallopOption[File] =
      trailArg[File](descr = "The previous Babel output", required = true)
    validateFileIsDirectory(babelOutput)
    validateFileIsDirectory(babelPrevOutput)

    val filterIn: ScallopOption[List[String]] = opt[List[String]](descr =
      "List of filenames to include (matched using startsWith)"
    )
    val filterOut: ScallopOption[List[String]] = opt[List[String]](descr =
      "List of filenames to exclude (matched using startsWith)"
    )

    val nCores: ScallopOption[Int] = opt[Int](descr = "Number of cores to use")

    val output: ScallopOption[File] = opt[File](descr = "Output file")
  }

  class Conf(args: Seq[String]) extends ScallopConf(args) {
    addSubcommand(new ValidateSubcommand())
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
      case Some(validate: ValidateSubcommand) => Reporter.diffResults(validate).exitCode
      case a => ZIO.fail(s"Error: no subcommand provided or invalid (${a})")
          .exitCode
    }
  }
}
