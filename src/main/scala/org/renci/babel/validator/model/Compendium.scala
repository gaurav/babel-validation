package org.renci.babel.validator.model

import com.typesafe.scalalogging.LazyLogging
import zio.ZIO
import zio.blocking.Blocking
import zio.stream._
import zio.json._

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

  case class Identifier (
                        i: Option[String],
                        l: Option[String]
                        )

  case class CompendiumRecord (
                                `type`: String,
                              ic: Option[Double],
                                identifiers: Seq[Identifier]
                              )

  implicit val identifierDecoder: JsonDecoder[Identifier] = DeriveJsonDecoder.gen[Identifier]
  implicit val recordDecoder: JsonDecoder[CompendiumRecord] = DeriveJsonDecoder.gen[CompendiumRecord]

  lazy val recordsRaw: ZStream[Blocking, Throwable, Either[String, CompendiumRecord]] = {
    lines.map(line => line.fromJson[CompendiumRecord])
  }

  lazy val records: ZStream[Blocking, Throwable, CompendiumRecord] = {
    lines
      .flatMap(line => line.fromJson[CompendiumRecord].fold(
      err => ZStream.fail(new RuntimeException(s"Could not parse line '${line}: ${err}")),
      r => ZStream.succeed(r)
    ))
  }

  // TODO: get rid of Summary, replace with direct calls to the wrapped object
  case class Summary(
    filename: String,
    file: File,
    countZIO: ZIO[Blocking, Throwable, Long],
    typesZIO: ZIO[Blocking, Throwable, Set[String]],
    typesZStream: ZStream[Blocking, Throwable, Either[String, CompendiumRecord]]
                    )

  def summary: Summary = Summary(
    filename,
    file,
    count,
    types,
    recordsRaw
  )

  def count: ZIO[Blocking, Throwable, Long] = lines.runCount
  def types: ZIO[Blocking, Throwable, Set[String]] = records.map(_.`type`).fold(Set[String]())(_ + _)
}
