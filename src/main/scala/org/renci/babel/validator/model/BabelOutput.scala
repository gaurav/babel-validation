package org.renci.babel.validator.model

import java.io.File

/** A BabelOutput is a directory containing Babel output results.
  */
class BabelOutput(root: File) {

  /** A description of this BabelOutput. */
  override def toString: String = {
    s"BabelOutput(${root}) containing ${compendia.length} compendia"
  }

  /** Return a list of all the files in a subdirectory of this BabelOutput.
    * @param dirName
    *   The subdirectory name.
    * @return
    *   The list of files in the {BabelOutput root}/{subdirectory}.
    */
  def getFilesInDir(dirName: String): Seq[String] = {
    val dir = new File(root, dirName)
    val filenames = dir.list()
    // TODO: this would be a good place to look for out-of-place files.
    filenames.toSeq
  }

  /** The compendia directory in this BabelOutput.
    */
  val compendiaDir: File = new File(root, "compendia")

  /** A list of all the compendia in this BabelOutput.
    */
  lazy val compendia: Seq[Compendium] =
    getFilesInDir("compendia").map(filename =>
      new Compendium(new File(compendiaDir, filename))
    )
}
