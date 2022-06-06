package org.renci.babel.validator

import com.typesafe.scalalogging.LazyLogging
import org.renci.babel.validator.Reporter.relativePercentChange
import org.renci.babel.validator.model.Compendium
import zio.ZIO

import java.io.PrintStream

/**
 * Methods in this class can be used to compare results between two compendia.
 */
object Comparer extends LazyLogging {
  /**
   * Our overall comparison method.
   */
  def compareTwoCompendia(output: PrintStream, filename: String, summary: Compendium, prevSummary: Compendium) = {
    for {
      count <- summary.count
      prevCount <- prevSummary.count
      typesChunk <- (for {
        row: Compendium.Record <- summary.records
      } yield (row.`type`)).runCollect
      typesErrors <- summary.types.catchAll(err => {
        logger.error(s"Types error: ${err}")
        ZIO.fail(err)
      })
      prevTypesChunk <- (for {
        row: Compendium.Record <- prevSummary.records
      } yield (row.`type`)).runCollect

      // types <- summary.typesZIO
      // prevTypes <- prevSummary.typesZIO
    } yield {
      output.println(
        s"${filename}\t${count}\t${prevCount}\t${relativePercentChange(count, prevCount)}"
      )

      val types = typesChunk.toSet
      val prevTypes = prevTypesChunk.toSet

      val added = types -- prevTypes
      val deleted = prevTypes -- types
      val changeString = (added.toSeq, deleted.toSeq) match {
        case (Seq(), Seq()) => "No change"
        case (added, Seq()) => s"Added: ${added}"
        case (Seq(), _) => s"Deleted: ${added}"
        case (added, deleted) =>
          s"Added: ${added}, Deleted: ${deleted}"
      }

      output.println(
        s"${filename}\t${types.mkString(", ")} (${typesChunk.length})\t${
          prevTypes
            .mkString(", ")
        } (${prevTypesChunk.length})\t${changeString}"
      )
    }
  }
}
