package processor

import org.scalatest.{Assertions, FunSuite}
import processor.YAMLProcessor.{Node, Mapping, Scalar, Sequence}

/**
 * @author vadim
 * @since 12.11.15
 */
class CollectionsTest extends FunSuite with Assertions {
  val f = new FsFixtures("from-spec", "collections")

  test("sequence of scalars") {
    assert(YAMLProcessor.parse(f.fixture("Example2.1.Sequence_of_Scalars")).get ==
      Sequence(Seq("Mark McGwire", "Sammy Sosa", "Ken Griffey")))
  }

  test("mapping scalars to scalars") {
    assert(YAMLProcessor.parse(f.fixture("Example2.2.Mapping_Scalars_to_Scalars")).get ==
      Mapping(Map(Scalar("hr") -> "65", Scalar("avg") -> "0.278", Scalar("rbi") -> "147")))
  }

  test("mapping scalars to sequences") {
    assert(YAMLProcessor.parse(f.fixture("Example2.3.Mapping_Scalars_to_Sequences")).get ==
      Mapping(Map(Scalar("american") -> Sequence(List("Boston Red Sox", "Detroit Tigers", "New York Yankees")),
        Scalar("national") -> Sequence(List("New York Mets", "Chicago Cubs", "Atlanta Braves")))))
  }

  test("sequence of mappings") {
    assert(YAMLProcessor.parse(f.fixture("Example2.4.Sequence_of_Mappings")).get ==
      Sequence(List(Mapping(Map(Scalar("name") -> "Mark McGwire", Scalar("hr") -> "65", Scalar("avg") -> "0.278")),
        Mapping(Map(Scalar("name") -> Scalar("Sammy Sosa"), Scalar("hr") -> "63", Scalar("avg") -> "0.288")))))
  }

  test("sequence of sequences") {
    assert(YAMLProcessor.parse(f.fixture("Example2.5.Sequence_of_Sequences")).get ==
      Sequence(List(Sequence(List("name", "hr", "avg")),
        Sequence(List("Mark McGwire", "65", "0.278")), Sequence(List("Sammy Sosa", "63", "0.288")))))
  }

  test("mapping of mappings") {
    assert(YAMLProcessor.parse(f.fixture("Example2.6.Mapping_of_Mappings")).get ==
      Mapping(Map(
        Scalar("Mark McGwire") -> Mapping(Map(Scalar("hr") -> "65", Scalar("avg") -> "0.278")),
        Scalar("Sammy Sosa") -> Mapping(Map(Scalar("hr") -> "63", Scalar("avg") -> "0.288")))
      )
    )
  }
}
