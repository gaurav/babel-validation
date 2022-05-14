package org.renci.babel.validator

import java.io.File

import com.typesafe.scalalogging._
import org.rogach.scallop._

object Validator extends App with LazyLogging {
  class Conf(args: Seq[String]) extends ScallopConf(args) {
    val babelOutput = trailArg[File](descr = "The current Babel output directory", required = true)
    val babelPrevOutput = trailArg[File](descr = "The previous Babel output", required = false)
    validateFileIsDirectory(babelOutput)
    validateFileIsDirectory(babelPrevOutput)
    verify()
  }

  val conf = new Conf(args)
  val babelOutput = conf.babelOutput()
  val babelPrevOutputOpt: Option[File] = conf.babelPrevOutput.toOption

  println(s"Profile of Babel output ${babelOutput}")
  println(babelOutput)

  if (!babelPrevOutputOpt.isEmpty) {
    val babelPrevOutput = babelPrevOutputOpt.get

    println(s"Profile of previous Babel output ${babelPrevOutput}")
    println(babelPrevOutput)

    println(s"Comparison of ${babelOutput} with previous ${babelPrevOutput}")
  }
}