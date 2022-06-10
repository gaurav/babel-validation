package org.renci.babel.utils

import org.rogach.scallop.{ScallopConfBase, ScallopOption}

object Utils {
  /**
   * Helper method for displaying the percent change between two counts.
   */
  def relativePercentChange(count: Long, countPrev: Long): String = {
    val percentChange = (count - countPrev).toDouble / countPrev * 100
    f"${count - countPrev}%+d\t$percentChange%+2.2f%%"
  }

  trait SupportsFilenameFiltering extends ScallopConfBase {
    val filterIn: ScallopOption[List[String]] = opt[List[String]](descr =
      "List of filenames to include (matched using startsWith)"
    )
    val filterOut: ScallopOption[List[String]] = opt[List[String]](descr =
      "List of filenames to exclude (matched using startsWith)"
    )
  }

  /**
   * Generic method to determine whether a particular filename should be
   * filtered in or out from the results. The algorithm we use is:
   * 1. If any `--filtered-in` prefixes are provided, then we exclude everything
   *    that isn't explicitly filtered in (by starting with one of those prefixes
   *    in a case-sensitive manner).
   * 2. Otherwise, all filenames are allowed EXCEPT those explicitly filtered out
   *    by `--filtered-out` by starting with one of those prefixes in a
   *    case-sensitive manner.
   */
  def filterFilename(conf: SupportsFilenameFiltering, filename: String): Boolean = {
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
}
