package org.renci.babel.validator.model

import com.typesafe.scalalogging.LazyLogging
import zio.ZIO
import zio.blocking.Blocking
import zio.stream._

import java.io.File
import scala.collection.mutable

// Q&D memorize from https://stackoverflow.com/a/36960228/27310
object Compendium {
  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }
}

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

  case class Summary(
    filename: String,
    file: File,
    countZIO: ZIO[Blocking, Throwable, Long]
  )

  def summary: Summary = Summary(
    filename,
    file,
    count
  )

  def count: ZIO[Blocking, Throwable, Long] = lines.runCount
}
