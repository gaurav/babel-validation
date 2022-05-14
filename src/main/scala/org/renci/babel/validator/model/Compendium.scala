package org.renci.babel.validator.model

import com.typesafe.scalalogging.LazyLogging
import zio.{Chunk, ZIO}
import zio.blocking.Blocking

import java.io.{File, FileInputStream, IOException}
import zio.stream._

import java.nio.file.Path

/**
 * A Compendium models a single compendium in a Babel output.
 *
 * At the moment, this is a JSON object with the following structure:
 *  {
 *    "type": "biolink:...",
 *    "identifiers": [{
 *      "i": "identifier",
 *      "l": "label"
 *    }, {
 *      ...
 *    }]
 *  }
 *
 *  Since these files can be VERY large, we should only process them in a stream
 *  if we need to generate any kind of summary statistics or to verify things.
 */
class Compendium(file: File) extends LazyLogging {
  val filename = file.getName
  val path = file.toPath

  lazy val lines: ZStream[Blocking, Throwable, String] = {
    ZStream.fromFile(path)
      .aggregate(ZTransducer.utf8Decode)
      .aggregate(ZTransducer.splitLines)
  }

  def count: ZIO[Blocking, Throwable, Long] = lines.runCount
}
