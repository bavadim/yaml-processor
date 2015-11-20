package processor

import java.io.File

import org.apache.commons.io.FileUtils

/**
 * @author vadim
 * @since 12.11.15
 */
class FsFixtures(category: String, subcategory: String) {
  def this(category: String) = this(category, "")

  private def file(fileName: String): File = {
    val classLoader = getClass.getClassLoader
    new File(classLoader.getResource(fileName).getFile)
  }

  private def fileAsStr(fileName: String): String = FileUtils.readFileToString(file(fileName))

  def fixture(name: String) = if (subcategory.isEmpty) fileAsStr(s"${category}/${name}.yaml")
                              else fileAsStr(s"${category}/${subcategory}/${name}.yaml")
}
