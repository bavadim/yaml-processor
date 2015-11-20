package processor

import org.scalatest.{FunSpec, Assertions, FunSuite}

/**
 * @author vadim
 * @since 18.11.15
 */
class SubparsersTest extends FunSpec with Assertions {
  describe("Basic Structures") {
    describe("Indentation Spaces") {
      it("Example 6.1. Indentation Spaces") {
        val p = YAMLProcessor.s_indent(1)
        assert(YAMLProcessor.parse(p,
          """  # Leading comment line spaces are
            |   # neither content nor indentation.
            |
            |Not indented:
            | By one space: |
            |    By four
            |      spaces
            | Flow style: [    # Leading spaces
            |   By two,        # in flow style
            |  Also by two,    # are neither
            |   Still by two   # content nor
            |    ]             # indentation.
          """.stripMargin).get == 1)
      }
    }
  }
}
