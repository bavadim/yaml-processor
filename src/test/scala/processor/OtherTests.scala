package processor

import org.scalatest.{Assertions, FunSuite}
import processor.YAMLProcessor._

/**
 * @author vadim
 * @since 02.11.15
 */
class OtherTests extends FunSuite with Assertions {
  val f = new FsFixtures("other")

  test("Empty inline map") {
    assert(Mapping(Map()) == YAMLProcessor.parse( """{}""").get)
  }

 test("Empty inline list") {
    assert(Sequence(List()) == YAMLProcessor.parse( """[]""").get)
  }

  test("simple list") {
    assert(Sequence(List(Scalar("item 1"), Scalar("item2"))) == YAMLProcessor.parse(
      """|
        				   |- item 1
        				   |- item2
        				   |""".stripMargin).get)
  }

  test("simple block map") {
    assert(Mapping(Map(Scalar("key1") -> Scalar("value 1"), Scalar("key2") -> Scalar("value2"))) == YAMLProcessor.parse(
      """|
        				   |key1: value 1
        				   |key2: value2
        				   |""".stripMargin).get)
  }

  test("simple flow map") {
    assert(
      Mapping(Map(Scalar("hr") -> Scalar("63"), Scalar("avg") -> Scalar("0.288"))) == YAMLProcessor.parse(
      """
        |{
        |     hr: 63,
        |   avg: 0.288
        |}
      """.stripMargin).get)
  }

  test("simple pair") {
      assert(Mapping(Map(Scalar("key") -> Scalar("value"))) == YAMLProcessor.parse("""key: value""").get)
  }

  test("With empty inline map") {
    assert(
      Sequence(List(Scalar("foo"), Mapping(Map()), Scalar("bar"))) ==
        YAMLProcessor.parse(
          """|- foo
            |- {}
            |- bar""".stripMargin).get)
  }

  test("empty inline list and map") {
    assert(
      Mapping(Map(Scalar("foo_bar") -> Scalar("true"), Scalar("snafu") -> Sequence(List()),
        Scalar("empty") -> Mapping(Map()))) ==
        YAMLProcessor.parse(
          """|foo_bar: true
            |snafu: []
            |empty: {}""".stripMargin).get)
  }

  test("list of list") {
    assert(Sequence(Seq(
      Sequence(Seq(Scalar("item11"), Scalar("item12"))),
      Sequence(Seq(Scalar("item21"), Scalar("item22")))
    )) ==
      YAMLProcessor.parse(
        """|
          						|-
          						| - item11
          						| - item12
          						|-
          						| - item21
          						| - item22
          						|""".stripMargin).get)
  }

  test("map of map pairs") {
    assert(Mapping(Map(
      Scalar("JFrame") -> Mapping(Map(
        Scalar("name") -> Scalar("myFrame"), Scalar("title") -> Scalar("My App Frame")
      ))
    )) ==
      YAMLProcessor.parse(
        """|
          |JFrame:
          | name: myFrame
          | title: My App Frame
          |""".stripMargin).get)
  }

  test("map of flow map") {
    assert(Mapping(Map(
      Scalar("Mark McGwire") -> Mapping(Map(Scalar("hr") -> Scalar("65"), Scalar("avg") -> Scalar("0.278"))),
      Scalar("Sammy Sosa") -> Mapping(Map(Scalar("hr") -> Scalar("63"), Scalar("avg") -> Scalar("0.288")))
    ))
      == YAMLProcessor.parse(
      """
        |Sammy Sosa: {
        |     hr: 63,
        |   avg: 0.288
        | }
        |Mark McGwire: {hr: 65, avg: 0.278}
      """.stripMargin).get)
  }

 test("list of map") {
    assert(Sequence(Seq(
      Mapping(Map(Scalar("name") -> Scalar("John Smith"), Scalar("age") -> Scalar("33"))),
      Mapping(Map(Scalar("name") -> Scalar("Mary Smith"), Scalar("age") -> Scalar("27"))))) ==
      YAMLProcessor.parse(
        """|
          						|- name: John Smith
          						|  age: 33
          						|- name: Mary Smith
          						|  age: 27
          						|""".stripMargin).get)
  }

  test("map of block list") {
    assert(Mapping(Map(
      Scalar("men") -> Sequence(Seq(Scalar("John Smith"), Scalar("Bill Jones"))),
      Scalar("women") -> Sequence(Seq(Scalar("Mary Smith"), Scalar("Susan Williams"))))) ==
      YAMLProcessor.parse(
        """|
          					   |men:
          					   | - John Smith
          					   | - Bill Jones
          					   |women:
          					   | - Mary Smith
          					   | - Susan Williams
          					   |""".stripMargin).get)
  }

  test("map of flow list") {
    assert(Mapping(Map(Scalar("Sammy Sosa") -> Sequence(Seq(Scalar("63"), Scalar("0.288")))))
      == YAMLProcessor.
      parse(
        """
          |Sammy Sosa: [
          |     63,
          |   0.288
          | ]
        """.stripMargin).get)
  }

  test("inline map") {
    assert(Mapping(Map(Scalar("key1") -> Scalar("value1"), Scalar("key2") -> Scalar("value2"))) == YAMLProcessor.parse(
      """{ key1: value1, key2: value2 }""").get)
  }

  test("map of map of map") {
    assert(
      Mapping(Map(
        Scalar("JFrame") -> Mapping(
          Map(
            Scalar("content") -> Mapping(
              Map(
                Scalar("button") -> Scalar("press")
              )
            )
          )
        )
      )) ==
      YAMLProcessor.
        parse(
          """|
                    |JFrame:
                    | content:
                    |  button: press
                    |""".stripMargin).get)
  }

  test("inline list") {
    assert(Sequence(List(Scalar("item1"), Scalar("item2"))) ==
      YAMLProcessor.parse("""[ item1, item2 ]""").get)
  }

  test("map of inline list") {
    assert(Mapping(Map(
      Scalar("men") -> Sequence(Seq(Scalar("John Smith"), Scalar("Bill Jones"))),
      Scalar("women") -> Sequence(Seq(Scalar("Mary Smith"), Scalar("Susan Williams"))))) ==
      YAMLProcessor.parse(
        """|
          					   |men: [ John Smith, Bill Jones ]
          					   |women: [ Mary Smith, Susan Williams ]
          					   |""".stripMargin).get)
  }

  test("more complicated") {
    assert(Mapping(
      Map(Scalar("address") -> Mapping(
        Map(Scalar("first_name") -> Scalar("Brian"),
          Scalar("last_name") -> Scalar("Reece"),
          Scalar("email") -> Scalar("brian@majordomo.com"),
          Scalar("company") -> Mapping(
            Map(Scalar("name") -> Scalar("Five Apart, Ltd."),
              Scalar("street_address") -> Scalar("8458 5th Street, San Francisco, CA 94107"))))))) == YAMLProcessor.parse(
        """|address:
          	  | first_name: Brian
          	  | last_name: Reece
          	  | email: brian@majordomo.com
          	  | company:
          	  |  name: Five Apart, Ltd.
          	  |  street_address: 8458 5th Street, San Francisco, CA 94107
        """.stripMargin).get)
  }

  test("comments") {
    assert(Mapping(Map(Scalar("hr") -> Sequence(Seq(Scalar("Mark McGwire"), "Sammy Sosa")), Scalar("rbi") -> Seq[Node](
      Scalar("Sammy Sosa"), "Ken Griffey"))) ==
      YAMLProcessor.parse(

        """hr: # 1998 hr ranking
      | - Mark McGwire
      | - Sammy Sosa
      |rbi:
      | # 1998 rbi ranking
      | - Sammy Sosa
      | - Ken Griffey
    """.stripMargin).get)
  }

  test("yet another comments") {
    assert(YAMLProcessor.parse(
      """hr:
                                 | - Mark McGwire
                                 | # Following node labeled SS
                                 | - SS Sammy Sosa
                                 |rbi:
                                 | - SS # Subsequent occurrence
                                 | - Ken Griffey""".
        stripMargin).get ==
      Mapping(Map(Scalar("hr") -> Sequence(Seq[Node]("Mark McGwire", "SS Sammy Sosa")),
      Scalar("rbi") -> Sequence(Seq[Node]("SS", "Ken Griffey"))))
    )
  }

  test("block skalars") {
    assert(
      YAMLProcessor.parse("""
      |name: Mark McGwire
      |accomplishment: >
      | Mark set a major league
      | home run record in 1998.
      |
      |stats: |
      |   65 Home Runs
      |   0.278 Batting Average
      |other: OK
      | """.stripMargin).get ==
      Mapping(Map(Scalar("name") -> Scalar("Mark McGwire"),
        Scalar("accomplishment") -> Scalar("Mark set a major league home run record in 1998."),
        Scalar("stats") -> Scalar("65 Home Runs\n0.278 Batting Average"), Scalar("other") -> "OK")))
  }

  test("block scalars (one literal style scalar)") {
    assert(
      YAMLProcessor.parse("""||
                            |  Mark set a major league
                            |  home run record in 1998.
                            | """.stripMargin).get == Scalar("Mark set a major league\nhome run record in 1998."))
  }


  test("block scalars (one folded style scalar)") {
    assert(
      YAMLProcessor.parse("""|>
                            |  Mark set a major league
                            |  home run record in 1998.
                            | """.stripMargin).get == Scalar("Mark set a major league home run record in 1998."))
  }
}

