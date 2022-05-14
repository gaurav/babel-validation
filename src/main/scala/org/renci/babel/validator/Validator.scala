package org.renci.babel.validator

import java.io.File
import com.typesafe.scalalogging._
import org.rogach.scallop._
import zio._
import org.renci.babel.validator.model.BabelOutput
import zio.blocking.Blocking
import zio.stream.ZSink

object Validator extends scala.App with LazyLogging {
  class Conf(args: Seq[String]) extends ScallopConf(args) {
    val babelOutput = trailArg[File](descr = "The current Babel output directory", required = true)
    val babelPrevOutput = trailArg[File](descr = "The previous Babel output", required = false)
    validateFileIsDirectory(babelOutput)
    validateFileIsDirectory(babelPrevOutput)
    verify()
  }

  val conf = new Conf(args)
  val babelOutput = new BabelOutput(conf.babelOutput())
  val babelPrevOutputOpt: Option[BabelOutput] = conf.babelPrevOutput.toOption.map(new BabelOutput(_))

  val runtime = Runtime.default

  println(s"Profile of Babel output ${babelOutput}")
  println(babelOutput)

  for {
    (name, counter) <- babelOutput.countCompendia
  } yield {
    val count = runtime.unsafeRun(counter)
    println(s"Number of lines in compendium ${name}: ${count}")
  }

  if (!babelPrevOutputOpt.isEmpty) {
    val babelPrevOutput = babelPrevOutputOpt.get

    println(s"Profile of previous Babel output ${babelPrevOutput}")
    println(babelPrevOutput)

    println(s"Comparison of ${babelOutput} with previous ${babelPrevOutput}")
  }
}