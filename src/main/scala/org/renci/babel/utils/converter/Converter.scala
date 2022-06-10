package org.renci.babel.utils.converter

import com.typesafe.scalalogging.LazyLogging
import org.renci.babel.utils.model.BabelOutput
import org.rogach.scallop.{ScallopOption, Subcommand}
import zio.ZIO
import zio.blocking.Blocking
import zio.console.Console
import zio.json._
import zio.stream.{ZSink, ZStream}

import java.io.File

/**
 * Convert Babel files into other formats.
 */
object Converter extends LazyLogging {
  /** The subcommand that controlling converting. */
  class ConvertSubcommand extends Subcommand("convert") {
    val babelOutput: ScallopOption[File] = trailArg[File](
      descr = "The current Babel output directory",
      required = true
    )
    validateFileIsDirectory(babelOutput)

    val format: ScallopOption[String] = choice(
      choices = Seq(
        "sssom"
      ),
      descr = "The format to convert this Babel output to",
      default = Some("sssom")
    )

    val filterIn: ScallopOption[List[String]] = opt[List[String]](descr =
      "List of filenames to include (matched using startsWith)"
    )
    val filterOut: ScallopOption[List[String]] = opt[List[String]](descr =
      "List of filenames to exclude (matched using startsWith)"
    )

    val nCores: ScallopOption[Int] = opt[Int](descr = "Number of cores to use")

    val output: ScallopOption[File] = opt[File](descr = "Output directory", required = true)
    validateFileIsDirectory(output)
  }

  def convert(conf: ConvertSubcommand): ZIO[Blocking with Console, Throwable, Unit] = {
    val babelOutput = new BabelOutput(conf.babelOutput())
    val outputDir = conf.output()

    val outputCompendia = new File(outputDir, "compendia")
    if (!outputCompendia.exists()) {
      outputCompendia.mkdirs()
    }

    val extension = conf.format() match {
      case "sssom" => ".tsv"
      case format => throw new RuntimeException(s"Unknown format: ${format}")
    }

    case class Other (
      subject_information_content: Option[Double]
    )
    implicit val otherEncoder: JsonEncoder[Other] =
      DeriveJsonEncoder.gen[Other]

    ZIO.foreach(babelOutput.compendia) { compendium =>
      val outputFilename = compendium.filename.replaceFirst("\\.\\w{1,3}$", extension)
      val outputFile = new File(outputCompendia, outputFilename)

      val results = compendium.records
        .flatMapPar(conf.nCores()) { record =>
          val cliqueLeader = record.identifiers.head
          val otherIdentifiers = record.identifiers.tail

          /*
          logger.debug(s"Record: ${record}")
          logger.debug(s" - Clique leader: ${cliqueLeader}")
          logger.debug(s" - Others: ${otherIdentifiers}")
          */

          val predicateId = "skos:exactMatch"
          val subjectString = s"${cliqueLeader.i.getOrElse("")}\t${cliqueLeader.l.getOrElse("")}\t${record.`type`}\t${predicateId}"

          val matchType = "HumanCurated"
          val other = Other(
            subject_information_content = record.ic
          )
          val otherString = s"${matchType}\t${other.toJson}"

          if (otherIdentifiers.isEmpty) {
            ZStream.fromIterable(Seq(
              s"${subjectString}\t\t\t${otherString}"
            ))
          } else {
            ZStream.fromIterable(otherIdentifiers)
              .map(obj => {
                val objectString = s"${obj.i.getOrElse("")}\t${obj.l.getOrElse("")}"
                s"${subjectString}\t${objectString}\t${otherString}"
              })
          }
        }

      (ZStream.fromIterable(Seq(
        s"subject_id\tsubject_label\tsubject_category\tpredicate_id\tobject_id\tobject_label\tobject_category\tmatch_type\tother"
      )) concat results)
        .intersperse("\n")
        .run({
          logger.info(s"Writing to ${outputFile}")
          ZSink.fromFile(outputFile.toPath)
            .contramapChunks[String](_.flatMap(_.getBytes))
        })
    }.andThen(ZIO.succeed())
  }
}
