package org.renci.babel.validator

import com.typesafe.scalalogging._
import org.renci.babel.validator.model.{BabelOutput, Compendium}
import org.rogach.scallop._
import zio._
import zio.blocking.Blocking
import zio.console._
import zio.stream.{ZSink, ZStream, ZTransducer}

import java.io.{File, FileOutputStream, PrintStream}
import scala.collection.Set
import scala.collection.immutable.Set

object Validator extends zio.App with LazyLogging {
  class Conf(args: Seq[String]) extends ScallopConf(args) {
    val babelOutput = trailArg[File](descr = "The current Babel output directory", required = true)
    val babelPrevOutput = trailArg[File](descr = "The previous Babel output", required = true)
    validateFileIsDirectory(babelOutput)
    validateFileIsDirectory(babelPrevOutput)

    val filterIn = opt[List[String]](descr = "List of filenames to include (matched using startsWith)")
    val filterOut = opt[List[String]](descr = "List of filenames to exclude (matched using startsWith)")

    val nCores = opt[Int](descr = "Number of cores to use")

    val output = opt[File](descr = "Output file")

    verify()
  }

  def filterFilename(conf: Conf, filename: String): Boolean = {
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

  def retrievePairedCompendiaSummaries(babelOutput: BabelOutput, babelPrevOutput: BabelOutput): Seq[(String, Compendium#Summary, Compendium#Summary)] = {
    for {
      summary <- babelOutput.compendiaSummary
      summaryPrev <- babelPrevOutput.compendiaSummary if summaryPrev.filename == summary.filename
    } yield {
      (summary.filename, summary, summaryPrev)
    }
  }

  def relativePercentChange(count: Long, countPrev: Long): String = {
    val percentChange = (count - countPrev).toDouble / countPrev * 100
    f"${count - countPrev}%+d\t$percentChange%+2.4f%%"
  }

  def diffResults(conf: Conf): ZIO[Blocking with Console, Throwable, Unit] = {
    val babelOutput = new BabelOutput(conf.babelOutput())
    val babelPrevOutput = new BabelOutput(conf.babelPrevOutput())
    val output = conf.output.toOption match {
      case Some(file) => new PrintStream(new FileOutputStream(file))
      case _ => System.out
    }

    /*
    val xyz = for {
      recs <- babelOutput.compendia.head.recordsRaw
    } yield {
      println(s"Record: ${recs}")
    }

    return xyz.runDrain
     */

    val pairedSummaries = retrievePairedCompendiaSummaries(babelOutput, babelPrevOutput)
    output.println("Filename\tCount\tPrevCount\tDiff\tPercentageChange")
    ZStream.fromIterable(pairedSummaries)
      .mapMParUnordered(conf.nCores())(result => result match {
        case (filename: String, summary: Compendium#Summary, prevSummary: Compendium#Summary) if filterFilename(conf, filename) => {
          for {
            count <- summary.countZIO
            prevCount <- prevSummary.countZIO
            typesChunk <- (for {
              row: Compendium#CompendiumRecord <- summary.typesZStream.collectRight
            } yield (row.`type`)).runCollect
            typesErrors <- summary.typesZStream.collectLeft.runCollect
            prevTypesChunk <- (for {
              row: Compendium#CompendiumRecord <- prevSummary.typesZStream.collectRight
            } yield (row.`type`)).runCollect

            // types <- summary.typesZIO
            // prevTypes <- prevSummary.typesZIO
          } yield {
            output.println(s"${filename}\t${count}\t${prevCount}\t${relativePercentChange(count, prevCount)}")

            if (typesErrors.nonEmpty) {
              logger.error(s"Types errors: ${typesErrors}")
            } else {
              val types = typesChunk.toSet
              val prevTypes = prevTypesChunk.toSet

              val added = types -- prevTypes
              val deleted = prevTypes -- types
              val changeString = (added.toSeq, deleted.toSeq) match {
                case (Seq(), Seq()) => "No change"
                case (added, Seq()) => s"Added: ${added}"
                case (Seq(), deleted) => s"Deleted: ${added}"
                case (added, deleted) => s"Added: ${added}, Deleted: ${deleted}"
              }

              output.println(s"${filename}\t${types.mkString(", ")} (${typesChunk.length})\t${prevTypes.mkString(", ")} (${prevTypesChunk.length})\t${changeString}")
            }
          }
        }
        case (filename: String, _, _) if !filterFilename(conf, filename) => {
          logger.info(s"Skipping ${filename}")
          ZIO.succeed()
        }
        case abc => ZIO.fail(new RuntimeException(s"Invalid paired summary: ${abc}"))
      })
      .runDrain
  }

  // TODO:
  // - Add processing time, preferably broken down by compendium or something (maybe just emit logs?)
  // - Some stats on memory usage would be great too

  /**
   * Entrypoint.
   *
   * @param args Command line arguments.
   */
  def run(args: List[String]) = {
    diffResults(new Conf(args)).exitCode
  }
}