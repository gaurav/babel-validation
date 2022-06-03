package org.renci.babel.validator.model

import java.io.File

/**
 * A BabelOutput is a directory containing Babel output results.
 */
class BabelOutput(root: File) {
  def getFilesInDir(dirName: String): Seq[String] = {
    val dir = new File(root, dirName)
    val filenames = dir.list()
    // TODO: this would be a good place to look for out-of-place files.
    filenames
  }

  val compendiaDir = new File(root, "compendia")
  lazy val compendia: Seq[Compendium] = getFilesInDir("compendia").map(filename => new Compendium(new File(compendiaDir, filename)))

  def compendiaSummary: Seq[Compendium#Summary] = compendia.map(_.summary)

  override def toString: String = {
    s"BabelOutput(${root}) containing ${compendia.length} compendia"
  }
}
