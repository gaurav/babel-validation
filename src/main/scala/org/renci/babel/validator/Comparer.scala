package org.renci.babel.validator

import com.typesafe.scalalogging.LazyLogging
import org.renci.babel.validator.model.Compendium
import zio.blocking.Blocking
import zio.{Chunk, ZIO}

/**
 * Methods in this class can be used to compare results between two compendia.
 */
object Comparer extends LazyLogging {
  /**
   * Helper method for displaying the percent change between two counts.
   */
  def relativePercentChange(count: Long, countPrev: Long): String = {
    val percentChange = (count - countPrev).toDouble / countPrev * 100
    f"${count - countPrev}%+d\t$percentChange%+2.2f%%"
  }

  case class LengthComparison(
      filename: String,
      count: Long,
      prevCount: Long) {
    val relativePercentChange: String = Comparer.relativePercentChange(count, prevCount)
    override val toString = s"${filename}\t${count}\t${prevCount}\t${relativePercentChange}"
  }

  def compareLengths(filename: String, summary: Compendium, prevSummary: Compendium): ZIO[Blocking, Throwable, LengthComparison] = {
    for {
      count <- summary.count
      prevCount <- prevSummary.count
    } yield LengthComparison(filename, count, prevCount)
  }

  case class TypeComparison(
      filename: String,
      types: Chunk[String],
      prevTypes: Chunk[String]
  ) {
    val typesSet = types.toSet
    val prevTypesSet = types.toSet
    val added = typesSet -- prevTypesSet
    val deleted = prevTypesSet -- typesSet
    val changeString = (added.toSeq, deleted.toSeq) match {
      case (Seq(), Seq()) => "No change"
      case (added, Seq()) => s"Added: ${added}"
      case (Seq(), _) => s"Deleted: ${added}"
      case (added, deleted) =>
        s"Added: ${added}, Deleted: ${deleted}"
    }

    override val toString = s"${filename}\t${typesSet.mkString(", ")} (${types.length})\t${
      prevTypesSet.mkString(", ")
    } (${prevTypes.length})\t${changeString}"
  }

  def compareTypes(filename: String, summary: Compendium, prevSummary: Compendium): ZIO[Blocking, Throwable, TypeComparison] = {
    for {
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
      prevTypesErrors <- prevSummary.types.catchAll(err => {
        logger.error(s"prevTypes error: ${err}")
        ZIO.fail(err)
      })
    } yield {
      TypeComparison(filename, typesChunk, prevTypesChunk)
    }
  }
}
