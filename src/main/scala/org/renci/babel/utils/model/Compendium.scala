package org.renci.babel.utils.model

import com.typesafe.scalalogging.LazyLogging
import org.renci.babel.utils.model.Compendium.{Identifier, Record}
import zio.ZIO
import zio.blocking.Blocking
import zio.json._
import zio.stream._

import java.io.File
import scala.collection.mutable

object Compendium extends LazyLogging {

  /**
   * Quick-and-dirty memoize() implementation from
   * https://stackoverflow.com/a/36960228/27310 This should probably be replaced
   * with ZIO Cache or ScalaCache at some point.
   *
   * @param f
   *   The function to memoize.
   * @tparam I
   *   The input type
   * @tparam O
   *   The output type
   * @return
   *   A function that will either return the cached value or calculate and
   *   cache it.
   */
  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I) = {
      logger.debug(s"Caching ${f}(${key}), already cached: ${contains(key)}")
      getOrElseUpdate(key, f(key))
    }
  }

  /** An identifier in this compendium. */
  case class Identifier(
      i: Option[String],
      l: Option[String]
  ) {
    override val toString: String = (i, l) match {
      case (None, None)       => s"None"
      case (Some(i), None)    => i
      case (None, Some(l))    => s"[${l}]"
      case (Some(i), Some(l)) => s"${i} [${l}]"
    }
  }

  /** A single record in this compendium. */
  case class Record(
      `type`: String,
      ic: Option[Double],
      identifiers: Seq[Identifier]
  ) {
    val primaryId: Option[String] = identifiers.headOption.flatMap(_.i)
    val ids: Set[String] = identifiers.flatMap(_.i).toSet
    override val toString: String = ic match {
      case None =>
        s"Record(${`type`} with ${identifiers.size} IDs: ${identifiers.mkString(", ")})"
      case Some(ic) =>
        s"Record(${`type`} [${ic}] with ${identifiers.size} IDs: ${identifiers.mkString(", ")})"
    }
  }
}

/**
 * A Compendium models a single compendium in a Babel output.
 *
 * At the moment, this is a JSON object with the following structure: { "type":
 * "biolink:...", "identifiers": [{ "i": "identifier", "l": "label" }, { ... }]
 * }
 */
class Compendium(file: File) extends LazyLogging {
  val filename = file.getName
  val path = file.toPath

  /**
   * A ZStream of all the lines in this file as strings.
   */
  lazy val lines: ZStream[Blocking, Throwable, String] = {
    ZStream
      .fromFile(path)
      .aggregate(ZTransducer.utf8Decode)
      .aggregate(ZTransducer.splitLines)
  }

  /* Implicit decoders for parts of the Record. */
  implicit val identifierDecoder: JsonDecoder[Identifier] =
    DeriveJsonDecoder.gen[Identifier]
  implicit val recordDecoder: JsonDecoder[Record] =
    DeriveJsonDecoder.gen[Record]

  /**
   * A ZStream that _doesn't_ throw an exception when you go through the
   * entries: instead, any record that can't be converted to a Record is kept as
   * an error as a String.
   */
  lazy val recordsRaw: ZStream[Blocking, Throwable, Either[String, Record]] = {
    lines.map(line => line.fromJson[Record])
  }

  /**
   * A ZStream of all the compendium records in this file. Throws an exception
   * if any line could not be converted into a String.
   */
  lazy val records: ZStream[Blocking, Throwable, Record] = {
    lines
      .flatMap(line =>
        line
          .fromJson[Record]
          .fold(
            err =>
              ZStream.fail(
                new RuntimeException(s"Could not parse line '${line}: ${err}")
              ),
            r => ZStream.succeed(r)
          )
      )
  }

  /** Count the total number of lines in this file. */
  def count: ZIO[Blocking, Throwable, Long] = lines.runCount

  /** Returns the Set of all the unique types in this file. */
  def types: ZIO[Blocking, Throwable, Set[String]] =
    records.map(_.`type`).fold(Set[String]())(_ + _)
}
