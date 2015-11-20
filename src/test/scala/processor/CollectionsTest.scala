package processor

import org.scalatest.{Assertions, FunSuite}

/**
 * @author vadim
 * @since 12.11.15
 */
class CollectionsTest extends FunSuite with Assertions {
  val f = new FsFixtures("from-spec", "collections")

  test("sequence of scalars") {
    assert(YAMLProcessor.parse(f.fixture("Example2.1.Sequence_of_Scalars")).get ==
      List("Mark McGwire", "Sammy Sosa", "Ken Griffey"))
  }

  test("mapping scalars to scalars") {
    assert(YAMLProcessor.parse(f.fixture("Example2.2.Mapping_Scalars_to_Scalars")).get ==
      Map("hr" -> "65", "avg" -> "0.278", "rbi" -> "147"))
  }

  test("mapping scalars to sequences") {
    assert(YAMLProcessor.parse(f.fixture("Example2.3.Mapping_Scalars_to_Sequences")).get ==
      Map("american" -> List("Boston Red Sox", "Detroit Tigers", "New York Yankees"),
        "national" -> List("New York Mets", "Chicago Cubs", "Atlanta Braves")))
  }

  test("sequence of mappings") {
    assert(YAMLProcessor.parse(f.fixture("Example2.4.Sequence_of_Mappings")).get ==
      List(Map("name" -> "Mark McGwire", "hr" -> "65", "avg" -> "0.278"),
        Map("name" -> "Sammy Sosa", "hr" -> "63", "avg" -> "0.288")))
  }

  test("sequence of sequences") {
    assert(YAMLProcessor.parse(f.fixture("Example2.5.Sequence_of_Sequences")).get ==
      List(List("name", "hr", "avg"), List("Mark McGwire", "65", "0.278"), List("Sammy Sosa", "63", "0.288")))
  }

  test("mapping of mappings") {
    assert(YAMLProcessor.parse(f.fixture("Example2.6.Mapping_of_Mappings")).get ==
      Map("Mark McGwire" -> Map("hr" -> "65", "avg" -> "0.278"), "Sammy Sosa" -> Map("hr" -> "63", "avg" -> "0.288")))
  }
}
