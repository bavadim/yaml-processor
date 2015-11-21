package processor

import scala.collection.immutable.Stream.Empty
import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.token.Tokens

/**
 * @author vadim
 * @since 12.11.15
 */
object YAMLProcessor extends RegexParsers with Tokens {
  override def skipWhitespace = false

  abstract class Node(val tag: String)
  case class Sequence(content: Seq[Node], override val tag: String = unresolved_tag) extends Node(tag) {
    override def toString = "[" + (content.foldRight("") {(e, acc) => acc + ", " + e.toString } + " ]").drop(1)
  }
  case class Mapping(content: Map[Node, Node], override val tag: String = unresolved_tag) extends Node(tag) {
    override def toString = "{" + (content.foldRight("") { (t, acc) => acc + ", " + t._1.toString + ": " + t._2.toString } + " }").drop(1)
  }
  case class Scalar(content: String, override val tag: String = unresolved_tag) extends Node(tag) {
    override def toString = "\"" + content + "\""
  }
  case class Empty(override val tag: String) extends Node(tag) {
    override def toString = "(" + tag + ")"
  }

  def emptyNodeParser(tag: String): PartialFunction[Option[Node], Node] = {
    case Some(v) => v
    case None => Empty(tag)
  }

  implicit class ParserExt[T](p: Parser[T]) {
    def `-`(p2: => Parser[T]): Parser[T] = Parser { in =>
      p(in) match {
        case Success(r, n) => p2(in) match {
          case Success(_, _)  => Failure("Expected failure", in)
          case _              => Success(r, n)
        }
        case other => other
      }
    }
  }

  val unresolved_tag = "!"

  //def parseSeqNode(l: List[Any], tag: String): List[Any] = l
  //def parseMap(m: Map[String, Any], tag: String): Map[String, Any] = m
  //def parseScalar(value: String): String = value

  val c_printable: Parser[String] = """[\p{Print}]""".r
  val c_byte_order_mark	=	"\uFEFF"
  val c_sequence_entry =	"-"
  val c_mapping_key	=	"?"
  val c_mapping_value	=	":"
  val c_collect_entry	=	","
  val c_sequence_start	=	"["
  val c_sequence_end	=	"]"
  val c_mapping_start	=	"{"
  val c_mapping_end	=	"}"
  val c_comment	=	"#"
  val c_anchor	=	"&"
  val c_alias	=	"*"
  val c_tag	=	"!"
  val c_literal	=	"|"
  val c_folded	=	">"
  val c_single_quote	=	"'"
  val c_double_quote	=	"\""
  val c_directive	=	"%"
  val c_reserved =	"@" | "`"
  val c_indicator	= "-" | "?" | ":" | "," | "[" | "]" | "{" | "}" |
    "#" | "&" | "*" | "!" | "|" | ">" | "'" | "\"" |
    "%" | "@" | "`"
  val c_flow_indicator	=	"," | "[" | "]" | "{" | "}"
  val b_break	= """[\r\n]|\r|\n""".r
  val b_as_line_feed	=	b_break
  val b_non_content	=	b_break
  val nb_char	=	c_printable - ("\r" | "\n" | c_byte_order_mark)
  val s_white: Parser[String] = """[ \t]""".r ^^^ " "
  val ns_char	=	nb_char - s_white
  val ns_dec_digit	=	"[0-9]".r ^^ (_.toInt)
  val ns_hex_digit	=	ns_dec_digit | "[a-fA-F]".r
  val ns_ascii_letter	= "[a-Z]"
  val ns_word_char =	ns_dec_digit | ns_ascii_letter | "-"
  val ns_uri_char	=	"%" ~ ns_hex_digit ~ ns_hex_digit ^^ {
    case _ ~ p2 ~ p3 => "%" + p2 + p3
  } | ns_word_char | "#" |
    ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" | "$" | "," |
    "_" | "." | "!" | "~" | "*" | "'" | "(" | ")" | "[" | "]"
  val ns_tag_char	=	ns_uri_char - "!" - c_flow_indicator
  val c_escape	=	"\\"
  val c_ns_esc_char	=	regex("""\\[/nvrxuUN_"]""".r) //TODO
  val nb_json = regex("""[\u0009,\u0020-\u10FF]""".r) //TODO
  val b_char = """[\n\r]""".r

  trait Context
  trait BlockContext extends Context
  trait FlowContext extends Context
  case object BlockIn extends BlockContext
  case object BlockOut extends BlockContext
  case object BlockKey extends FlowContext
  case object FlowKey extends FlowContext
  case object FlowIn extends FlowContext
  case object FlowOut extends FlowContext

  def s_indent(n: Int): Parser[Int] = (s"^[ ]{$n}"r) ^^ { _.length }
  def s_indentGreaterZ(n: Int): Parser[Int] =
    if (n == -1) s"^[ ]*".r ^^ { _.length } else s"^[ ]{${n + 1},}".r ^^ { _.length }
  val s_indentCount: Parser[Int] = s"^[ ]*".r ^^ { _.length }
  def s_indentLess(n: Int): Parser[Int] = if (n-1 > 0) (s"^[ ]{0,${n-1}}"r) ^^ { _.length } else Parser {in => Failure("", in) }
  def s_indentLessOrEq(n: Int): Parser[Int] = (s"^[ ]{0,$n}"r) ^^ { _.length }
  val s_separate_in_line	=	regex("[ \t]+|$]".r) /* Start of line */
  def s_block_line_prefix(n: Int): Parser[Unit] = s_indent(n) ^^ (_ => ())
  def s_flow_line_prefix(n: Int): Parser[Unit] =	(s_indent(n) ~ s_separate_in_line.?) ^^ (_ => ())
  def s_line_prefix(n: Int,c: Context): Parser[Unit] = c match {
    case BlockOut => s_block_line_prefix(n)
    case BlockIn => s_block_line_prefix(n)
    case FlowOut => s_flow_line_prefix(n)
    case FlowIn => s_flow_line_prefix(n)
    case _ => throw new Exception("Wrong BNF")
  }
  def l_empty(n: Int, c: Context): Parser[String] =	( s_line_prefix(n,c) | s_indentLess(n) ) ~> b_as_line_feed
  def b_l_trimmed(n: Int,c: Context): Parser[String] =	b_non_content ~> l_empty(n,c).+ ^^ (_.mkString)
  val b_as_space: Parser[String] = b_break ^^ (_ => " ")
  def b_l_folded(n: Int, c: Context): Parser[String] =	b_l_trimmed(n,c) | b_as_space
  def s_flow_folded(n: Int): Parser[String]	=	s_separate_in_line.? ~> b_l_folded(n, FlowIn) <~ s_flow_line_prefix(n)
  //Comments
  val c_nb_comment_text: Parser[Unit]	=	"#" ~> nb_char.* ^^ (_ => ())
  val b_comment	=	b_non_content //TODO EOF
  val s_b_comment	=	( s_separate_in_line ~> c_nb_comment_text? ).? ~> b_comment
  val l_comment	=	s_separate_in_line ~> c_nb_comment_text.? ~> b_comment
  val s_l_comments: Parser[Unit] =	(s_b_comment | "^".r /* Start of line */ ) ~> l_comment.* ^^ (_ => ())
  //Separation Lines
  def s_separate_lines(n: Int): Parser[Unit]	=	(s_l_comments ~> s_flow_line_prefix(n)) | s_separate_in_line ^^ (_ => ())
  def s_separate(n: Int, c: Context): Parser[Unit] = (c match {
    case BlockOut => s_separate_lines(n)
    case BlockIn => s_separate_lines(n)
    case FlowOut => s_separate_lines(n)
    case FlowIn => s_separate_lines(n)
    case BlockKey => s_separate_in_line
    case FlowKey => s_separate_in_line
  }) ^^ (_ => ())
  //Directives
  val ns_directive_parameter =	ns_char+
  val ns_directive_name	=	ns_char+
  val ns_reserved_directive	=	ns_directive_name ~ (s_separate_in_line <~ ns_directive_parameter)*
  val ns_yaml_version	=	(ns_dec_digit.+ ^^ { case d =>
    if (d.mkString.toInt > 1) throw new IllegalArgumentException(s"YAML version $d not supported!")
    else d
  }) ~ "." ~ (ns_dec_digit.+ ^^ { case d =>
    if (d.mkString.toInt > 2) println(s"Warning: attempt parsing YAML version $d")
    d
  })
  val ns_yaml_directive	=	"YAML" ~ s_separate_in_line ~> ns_yaml_version
  val c_primary_tag_handle =	"!"
  val c_secondary_tag_handle =	"!!"
  val c_named_tag_handle = "!" ~> ns_word_char.+  <~ "!" ^^ {_.mkString}
  val c_tag_handle = c_named_tag_handle | c_secondary_tag_handle | c_primary_tag_handle
  val c_ns_local_tag_prefix	=	"!" ~> ns_uri_char.* ^^ (_.mkString)
  val ns_global_tag_prefix =	ns_tag_char ~> ns_uri_char.* ^^ (_.mkString)
  val ns_tag_prefix	=	c_ns_local_tag_prefix | ns_global_tag_prefix
  val ns_tag_directive =	"TAG" ~ s_separate_in_line ~> c_tag_handle ~ s_separate_in_line ~ ns_tag_prefix
  val l_directive: Parser[Unit]	=	"%" ~> (ns_yaml_directive | ns_tag_directive | ns_reserved_directive) <~ s_l_comments ^^ (_ => ())
  //Node Properties
  val c_verbatim_tag: Parser[String] = "!<" ~> ns_uri_char.+ <~ ">" ^^ (_.mkString)
  val c_ns_shorthand_tag: Parser[String] = c_tag_handle ~> ns_tag_char.+ ^^ (_.mkString)
  val c_non_specific_tag: Parser[String] = "!"
  val ns_anchor_char =	ns_char - c_flow_indicator
  val ns_anchor_name = ns_anchor_char.+ ^^ (_.mkString)
  val c_ns_anchor_property: Parser[String] =	"&" ~> ns_anchor_name
  val c_ns_tag_property: Parser[String]	=	c_verbatim_tag | c_ns_shorthand_tag | c_non_specific_tag
  def c_ns_properties(n: Int, c: Context): Parser[(String, String)]	=
    (c_ns_tag_property ~ opt(s_separate(n,c) ~> c_ns_anchor_property) ^^ {
      case tag ~ Some(anchor) => (tag, anchor)
      case tag ~ None => (tag, "")
    }) |
      (c_ns_anchor_property ~ opt(s_separate(n,c) ~> c_ns_tag_property) ^^ {
        case anchor ~ Some(tag) => (tag, anchor)
        case anchor ~ None => ("!", anchor)
      })
  //Alias Nodes
  val c_ns_alias_node: Parser[Node]	=	"*" ~ ns_anchor_name ^^ {_ =>
    println("Aliases not supported yet!")
    Scalar("Aliases not supported yet!", unresolved_tag)
  }
  //Empty Nodes
  //val e_scalar: Parser[String] = "" ^^ (_ => "") //TODO
  //val e_node: Parser[Node]	=	e_scalar ^^ (_ => Scalar("", unresolved_tag))
  //Flow Scalar Styles
  val nb_double_char = c_ns_esc_char | (nb_json - "\\" - "\"" )
  val ns_double_char = nb_double_char - s_white
  val nb_double_one_line = nb_double_char.* ^^ (_.mkString)
  def s_double_escaped(n: Int): Parser[String] =
    (s_white.* ^^ (_.mkString)) ~ "\\" ~ b_non_content ~ (l_empty(n, FlowIn).* ^^ (_.mkString)) <~ s_flow_line_prefix(n) ^^ {
      case s1 ~ _ ~ s2 => s1 + s2
    }
  def s_double_break(n: Int): Parser[String] = s_double_escaped(n) | s_flow_folded(n)
  val nb_ns_double_in_line: Parser[String] =
    ((s_white.* ^^ (_.mkString)) ~ ns_double_char ^^ {case s1 ~ s2 => s1 + s2}).* ^^ (_.mkString)
  def s_double_next_line(n: Int): Parser[String] = s_double_break(n) ~ opt(ns_double_char ~ nb_ns_double_in_line ~
    (s_double_next_line(n) | (s_white.* ^^ (_.mkString))) ^^ {case s1 ~ s2 ~ s3 => s1 + s2 + s3}) ^^ {
    case s1 ~ Some(s2) => s1 + s2
    case s1 ~ None => s1
  }
  def nb_double_multi_line(n: Int): Parser[String] =
    nb_ns_double_in_line ~ (s_double_next_line(n) | (s_white.* ^^ (_.mkString))) ^^ {case s1 ~ s2 => s1 + s2}
  def nb_double_text(n: Int, c: Context): Parser[String] = c match {
    case FlowOut => nb_double_multi_line(n)
    case FlowIn => nb_double_multi_line(n)
    case BlockKey => nb_double_one_line
    case FlowKey => nb_double_one_line
    case _ => throw new Exception("Wrong BNF")
  }
  def c_double_quoted(n: Int, c: Context): Parser[String]	=	"\"" ~> nb_double_text(n,c) <~ "\""
  val c_quoted_quote = "''"
  val nb_single_char: Parser[String] = c_quoted_quote | (nb_json - "'")
  val ns_single_char: Parser[String] = nb_single_char - s_white
  val nb_single_one_line: Parser[String] = nb_single_char.* ^^ (_.mkString)
  val nb_ns_single_in_line: Parser[String] = (s_white.* ~> ns_single_char).* ^^ (_.mkString)
  def s_single_next_line(n: Int): Parser[String] = s_flow_folded(n) ~>
    opt(ns_single_char ~ nb_ns_single_in_line ~ (s_single_next_line(n) | (s_white.* ^^ (_.mkString)))) ^^ {
    case Some(s1 ~ s2 ~ s3) => s1 + s2 + s3
    case None => ""
  }
  def nb_single_multi_line(n: Int): Parser[String] = nb_ns_single_in_line ~
    (s_single_next_line(n) | (s_white.* ^^ (_.mkString))) ^^ { case s1 ~ s2 => s1 + s2}
  def nb_single_text(n: Int, c: Context): Parser[String] = c match {
    case FlowOut => nb_single_multi_line(n)
    case FlowIn => nb_single_multi_line(n)
    case BlockKey => nb_single_one_line
    case FlowKey => nb_single_one_line
    case _ => throw new Exception("Wrong BNF")
  }
  def c_single_quoted(n: Int, c: Context): Parser[String]	=	"'" ~> nb_single_text(n,c) <~ "'"
  def ns_plain_first(c: Context) = (ns_char - c_indicator) //| ("(?<=\w)[?:-]"r /* Followed by an ns-plain-safe(c)) */ )
  val ns_plain_safe_out =	ns_char
  val ns_plain_safe_in = ns_char - c_flow_indicator
  def ns_plain_char(c: Context): Parser[String] = (ns_plain_safe(c) - ":" - "#") //| ( /* An ns-char preceding */ "#") | (":" /* Followed by an ns-plain-safe(c) */ )
  def ns_plain_safe(c: Context): Parser[String]	=	c match {
    case FlowOut => ns_plain_safe_out
    case FlowIn => ns_plain_safe_in
    case BlockKey => ns_plain_safe_out
    case FlowKey => ns_plain_safe_in
    case _ => throw new Exception("Wrong BNF")

  }
  def nb_ns_plain_in_line(c: Context): Parser[String] = (s_white.* ~ ns_plain_char(c) ^^ { case wl ~ ch => wl.mkString + ch }).* ^^ (_.mkString)
  def ns_plain_one_line(c: Context): Parser[String]	=	ns_plain_first(c) ~ nb_ns_plain_in_line(c) ^^ {case s1 ~ s2 => s1 + s2}
  def s_ns_plain_next_line(n: Int, c: Context) = s_flow_folded(n) ~> ns_plain_char(c) ~ nb_ns_plain_in_line(c) ^^ {case s1 ~ s2 => s1 + s2}
  def ns_plain_multi_line(n: Int, c: Context): Parser[String] =
    ns_plain_one_line(c) ~ (s_ns_plain_next_line(n,c).* ^^ (_.mkString)) ^^ {case s1 ~ s2 => s1 + s2}
  def ns_plain(n: Int, c: Context): Parser[String] = c match {
    case FlowOut => ns_plain_multi_line(n,c)
    case FlowIn => ns_plain_multi_line(n,c)
    case BlockKey => ns_plain_one_line(c)
    case FlowKey => ns_plain_one_line(c)
    case _ => throw new Exception("Wrong BNF")
  }
  //Flow Collection Styles
  def in_flow(c: Context): Context = c match {
    case FlowOut => FlowIn
    case FlowIn => FlowIn
    case BlockKey => FlowKey
    case FlowKey => FlowKey
    case _ => throw new Exception("Wrong BNF")
  }
  //Flow mappings
  def c_ns_flow_map_adjacent_value(n: Int, c: Context): Parser[Node] = ":" ~> (opt(s_separate(n, c).? ~> ns_flow_node(n, c)) ^^ emptyNodeParser("c_ns_flow_map_adjacent_value") /*| e_node*/ ) /* Value */
  def c_s_implicit_json_key(c: Context): Parser[Node] = c_flow_json_node(0, c) <~ s_separate_in_line.?
  /* At most 1024 characters altogether */
  def ns_flow_map_yaml_key_entry(n: Int, c: Context): Parser[(Node, Node)] =
    ns_flow_yaml_node(n,c) ~ (opt(s_separate(n,c).? ~> c_ns_flow_map_separate_value(n,c)) ^^ emptyNodeParser("ns_flow_map_yaml_key_entry") /*| e_node*/) ^^ { case k ~ v => (k, v) }
  def c_ns_flow_map_json_key_entry(n: Int, c: Context): Parser[(Node, Node)] = c_flow_json_node(n,c) ~ ((s_separate(n,c).? ~>
    c_ns_flow_map_adjacent_value(n,c)) | e_node) ^^ {case k ~ v => k -> v }
  def ns_flow_map_implicit_entry(n: Int, c: Context): Parser[(Node, Node)] = ns_flow_map_yaml_key_entry(n,c) |
    c_ns_flow_map_empty_key_entry(n,c) | c_ns_flow_map_json_key_entry(n,c)
  def ns_flow_map_explicit_entry(n: Int, c: Context): Parser[(Node, Node)] = ns_flow_map_implicit_entry(n,c) |
    ( e_node /* Key */ ~ e_node /* Value */ ^^ {case k ~ v => k -> v } )
  def ns_s_implicit_yaml_key(c: Context): Parser[Node] = ns_flow_yaml_node(0,c) <~ s_separate_in_line.?
  def ns_flow_pair_yaml_key_entry(n: Int, c: Context): Parser[(Node, Node)] =
    ns_s_implicit_yaml_key(FlowKey) ~ c_ns_flow_map_separate_value(n,c) ^^ { case k ~ v => (k, v) }
  def c_ns_flow_pair_json_key_entry(n: Int, c: Context): Parser[(Node, Node)] =
    c_s_implicit_json_key(FlowKey) ~ c_ns_flow_map_adjacent_value(n,c) ^^ { case k ~ v => (k, v) }
  def ns_flow_pair_entry(n: Int, c: Context): Parser[(Node, Node)] = ns_flow_pair_yaml_key_entry(n,c) |
    c_ns_flow_map_empty_key_entry(n,c) | c_ns_flow_pair_json_key_entry(n,c)
  def ns_flow_pair(n: Int, c: Context): Parser[(Node, Node)] =
    ("?" ~ s_separate(n,c) ~> ns_flow_map_explicit_entry(n,c)) | ns_flow_pair_entry(n, c)
  def c_ns_flow_map_separate_value(n: Int, c: Context): Parser[Node] = ":" /* Not followed by an ns-plain-safe(c) */ ~>
    (( s_separate(n,c) ~> ns_flow_node(n,c) ) | e_node /* Value */ )
  def c_ns_flow_map_empty_key_entry(n: Int, c: Context): Parser[(Node, Node)] =
    e_node /* Key */ ~ c_ns_flow_map_separate_value(n,c) ^^ {case k ~ v => k -> v }
  def ns_flow_map_entry(n: Int, c: Context): Parser[(Node, Node)] =
    ("?" ~ s_separate(n,c) ~> ns_flow_map_explicit_entry(n,c)) | ns_flow_map_implicit_entry(n,c)
  def ns_s_flow_map_entries(n: Int, c: Context): Parser[Map[Node, Node]] = ns_flow_map_entry(n,c) ~ s_separate(n,c).? ~
    ("," ~ s_separate(n,c).? ~> ns_s_flow_map_entries(n,c).? ).? ^^ {
    case h ~ _ ~ Some(Some(t)) => t + h
    case h ~ _ ~ _ => Map(h)
  }
  def c_flow_mapping(n: Int, c: Context): Parser[Map[Node, Node]] =
    "{" ~ s_separate(n, c).? ~> ns_s_flow_map_entries(n, in_flow(c)).?  <~ "}" ^^ (_.getOrElse(Map()))
  //Flow seq
  def ns_flow_seq_entry(n: Int,c: Context): Parser[Node] = (ns_flow_pair(n,c) ^^ {m => Mapping(Map(m), unresolved_tag)}) | ns_flow_node(n,c)
  def ns_s_flow_seq_entries(n: Int, c: Context): Parser[Seq[Node]] =
    ns_flow_seq_entry(n,c) ~ s_separate(n,c).? ~ ("," ~ s_separate(n,c).? ~> ns_s_flow_seq_entries(n,c).?).? ^^ {
      case ent ~ _ ~ Some(Some(t)) => t.+:(ent)
      case ent ~ _ ~ _ => Seq(ent)
    }
  def c_flow_sequence(n: Int, c: Context): Parser[Seq[Node]] =
    "[" ~ s_separate(n,c).? ~> ns_s_flow_seq_entries(n, in_flow(c)).?  <~ "]" ^^ (_.getOrElse(Seq()))
  def ns_flow_yaml_content(n: Int, c: Context): Parser[String] = ns_plain(n,c)
  //Flow nodes
  def c_flow_json_content(n: Int, c: Context, prop: Option[(String, String)]): Parser[Node] =
    c_flow_sequence(n, c) ^^ (Sequence(_, prop.map(_._1).getOrElse(unresolved_tag))) | c_flow_mapping(n, c) ^^ (Mapping(_, prop.map(_._1).getOrElse(unresolved_tag))) |
      (c_single_quoted(n, c) | c_double_quoted(n, c)) ^^ (Scalar(_, prop.map(_._1).getOrElse(unresolved_tag)))
  def ns_flow_content(n: Int, c: Context, prop: Option[(String, String)]): Parser[Node] =
    (ns_flow_yaml_content(n,c) ^^ (Scalar(_, prop.map(_._1).getOrElse(unresolved_tag)))) | c_flow_json_content(n,c, prop)
  def ns_flow_yaml_node(n: Int, c: Context): Parser[Node] =
    c_ns_alias_node | (ns_flow_yaml_content(n, c) ^^ (Scalar(_, unresolved_tag))) |
      c_ns_properties(n, c) >> { case (tag, anchor) => //TODO anchor
        ((s_separate(n,c) ~> ns_flow_yaml_content(n,c)) | e_scalar) ^^ (Scalar(_, tag))
      }
  def c_flow_json_node(n: Int, c: Context): Parser[Node] =
    (c_ns_properties(n, c) <~ s_separate(n, c)).? >> (c_flow_json_content(n, c, _))
  def ns_flow_node(n: Int, c: Context): Parser[Node] =
    c_ns_alias_node | ns_flow_content(n, c, None) |
      (c_ns_properties(n,c) >> { props =>
        (s_separate(n,c) ~> ns_flow_content(n,c, Some(props))) | e_scalar ^^ (Scalar(_, props._1))
      })
  //Block Styles
  // Block Scalar Styles
  // Block Scalar Headers
  trait BlockChompingIndicator
  case object Strip extends BlockChompingIndicator
  case object Keep extends BlockChompingIndicator
  case object Clip extends BlockChompingIndicator

  def c_b_block_header: Parser[(Int, BlockChompingIndicator)] = //TODO
    ((c_indentation_indicator ~ c_chomping_indicator) ^^ { case i ~ c => (i, c) } |
    (c_chomping_indicator ~ c_indentation_indicator) ^^ { case c ~ i => (i, c) }) <~ s_b_comment
  def c_indentation_indicator: Parser[Int]	=	ns_dec_digit | s_indentCount
  def c_chomping_indicator: Parser[BlockChompingIndicator] = opt(("-" ^^^ Strip) | ("+" ^^^ Keep)) ^^ {
    case Some(c) => c
    case None => Clip
  }
  def b_chomped_last(t: BlockChompingIndicator) =	t match {
    case Strip => b_non_content /* TODO | End of file */
    case Clip => b_as_line_feed /* TODO | End of file */
    case Keep => b_as_line_feed /* TODO | End of file */
  }
  def l_chomped_empty(n: Int, t: BlockChompingIndicator): Parser[String] = t match {
    case Strip => l_strip_empty(n)
    case Clip => l_strip_empty(n)
    case Keep => l_keep_empty(n)
  }
  def l_strip_empty(n: Int): Parser[String] =	(s_indentLessOrEq(n) ~ b_non_content).* <~ l_trail_comments(n).? ^^^ ""
  def l_keep_empty(n: Int): Parser[String] = (l_empty(n, BlockIn).* ^^ (_.mkString)) <~ l_trail_comments(n).?
  def l_trail_comments(n: Int) = s_indentLess(n) ~ c_nb_comment_text ~ b_comment ~ l_comment*
  //Literal Style
  def c_l_literal(n: Int): Parser[String] = "|" ~> c_b_block_header >> { case (m, t) => l_literal_content(n + m, t) }
  def l_nb_literal_text(n: Int): Parser[String] = (l_empty(n, BlockIn).* ^^ (_.mkString)) ~ s_indent(n) ~ nb_char.+ ^^ {
    case s1 ~ _ ~ o => s1 + o.mkString
  }
  def b_nb_literal_next(n: Int): Parser[String] = b_as_line_feed ~ l_nb_literal_text(n) ^^ { case s1 ~ s2 => s1 + s2 }
  def l_literal_content(n: Int, t: BlockChompingIndicator): Parser[String] = (l_nb_literal_text(n) ~
    b_nb_literal_next(n).* ~ b_chomped_last(t) ^^ {case s1 ~ l ~ s2 => s1 + l.mkString + s2} ).? ~ l_chomped_empty(n,t) ^^ {
    case Some(s1) ~ s2 => s1 + s2
    case None ~ s => s
  }
  //Folded Style
  def c_l_folded(n: Int): Parser[String] = ">" ~> c_b_block_header >> { case (m, t) => l_folded_content(n + m, t) }
  def s_nb_folded_text(n: Int): Parser[String] = s_indent(n) ~> ns_char ~ nb_char.* ^^ { case c ~ l => c + l.mkString }
  def l_nb_folded_lines(n: Int): Parser[String] = s_nb_folded_text(n) ~ (b_l_folded(n, BlockIn) ~> s_nb_folded_text(n)).* ^^
    { case s1 ~ l => s1 + l.mkString }
  def s_nb_spaced_text(n: Int): Parser[String] = s_indent(n) ~ s_white ~> nb_char.* ^^ (_.mkString)
  def b_l_spaced(n: Int): Parser[String] = b_as_line_feed ~ l_empty(n, BlockIn).* ^^^ " "
  def l_nb_spaced_lines(n: Int): Parser[String] = s_nb_spaced_text(n) ~ ((b_l_spaced(n) ~ s_nb_spaced_text(n)).* ^^^ " ") ^^
    { case s1 ~ s2 => s1 + s2 }
  def l_nb_same_lines(n: Int): Parser[String] = l_empty(n, BlockIn).* ~> (l_nb_folded_lines(n) | l_nb_spaced_lines(n))
  def l_nb_diff_lines(n: Int): Parser[String] = l_nb_same_lines(n) ~
    (b_as_line_feed ~ l_nb_same_lines(n) ^^ {case s1 ~ s2 => s1 + s2}).* ^^ { case s1 ~ l => s1 + l.mkString}
  def l_folded_content(n: Int, t: BlockChompingIndicator): Parser[String] =
    (l_nb_diff_lines(n) ~ b_chomped_last(t) ^^ {case s1 ~ s2 => s1 + s2}).? ~ l_chomped_empty(n,t) ^^ {
      case None ~ s => s
      case Some(s1) ~ s2 => s1 + s2
    }
  //Block Collection Styles
  //Block Sequences
  def l_block_sequence(n: Int, props: Option[(String, String)]): Parser[Sequence] = (s_indentGreaterZ(n) >> { m =>
    c_l_block_seq_entry(m)
  }).+ ^^ (Sequence(_, props.map(t => t._1).getOrElse(unresolved_tag)))
  def c_l_block_seq_entry(n: Int): Parser[Node] = "-" /* Not followed by an ns-char */ ~> s_l_block_indented(n, BlockIn)
  def s_l_block_indented(n: Int, c: Context): Parser[Node] = (s_indentCount >> { m =>
    ns_l_compact_sequence(n + 1 + m, None) | ns_l_compact_mapping(n + 1 + m, None) }) | s_l_block_node(n,c) |
    ( e_node <~ s_l_comments )
  def ns_l_compact_sequence(n: Int, props: Option[(String, String)]): Parser[Sequence] = c_l_block_seq_entry(n) ~
    (s_indent(n) ~> c_l_block_seq_entry(n)).* ^^ { case h ~ l => Sequence(h :: l, props.map(_._1).getOrElse(unresolved_tag))}
  //Block Mappings
  def l_block_mapping(n: Int, props: Option[(String, String)]): Parser[Mapping] =
    (s_indentGreaterZ(n) >> { m => ns_l_block_map_entry(m) }).+ ^^ { p => Mapping(p.toMap, props.map(_._1).getOrElse(unresolved_tag)) }
  def ns_l_block_map_entry(n: Int): Parser[(Node, Node)] = c_l_block_map_explicit_entry(n) | ns_l_block_map_implicit_entry(n)
  def c_l_block_map_explicit_entry(n: Int): Parser[(Node, Node)] =
    c_l_block_map_explicit_key(n) ~ (l_block_map_explicit_value(n) | e_node) ^^ { case k ~ v => (k, v) }
  def c_l_block_map_explicit_key(n: Int) = "?" ~> s_l_block_indented(n, BlockOut)
  def l_block_map_explicit_value(n: Int): Parser[Node] = s_indent(n) ~ ":" ~> s_l_block_indented(n, BlockOut)
  def ns_l_block_map_implicit_entry(n: Int): Parser[(Node, Node)] =
    (ns_s_block_map_implicit_key | e_node) ~ c_l_block_map_implicit_value(n) ^^ { case k ~ v => (k, v) }
  def ns_s_block_map_implicit_key = c_s_implicit_json_key(BlockKey) | ns_s_implicit_yaml_key(BlockKey)
  def c_l_block_map_implicit_value(n: Int): Parser[Node] = ":" ~> (s_l_block_node(n, BlockOut) | (e_node <~ s_l_comments))
  def ns_l_compact_mapping(n: Int, props: Option[(String, String)]): Parser[Mapping] =
    ns_l_block_map_entry(n) ~ (s_indent(n) ~> ns_l_block_map_entry(n)).* ^^ {
      case h ~ t => Mapping((h :: t).toMap, props.map(_._1).getOrElse(unresolved_tag))
    }
  //Block Nodes
  def s_l_block_node(n: Int, c: Context): Parser[Node] = s_l_block_in_block(n, c) | s_l_flow_in_block(n)
  def s_l_flow_in_block(n: Int): Parser[Node] = s_separate(n + 1, FlowOut) ~> ns_flow_node(n + 1, FlowOut) <~ s_l_comments
  def s_l_block_in_block(n: Int, c: Context): Parser[Node] = s_l_block_scalar(n, c) | s_l_block_collection(n, c)
  def s_l_block_scalar(n: Int, c: Context): Parser[Scalar] = s_separate(n + 1, c) ~>
    (c_ns_properties(n + 1, c) <~ s_separate(n + 1, c)).? >> { o =>
    (c_l_literal(n) | c_l_folded(n)) ^^ (Scalar(_, o.map(op => op._1).getOrElse(unresolved_tag)))
  }
  def s_l_block_collection(n: Int, c: Context): Parser[Node] = {
    def seq_spaces(n: Int, c: Context): Int = c match {
      case BlockOut => n - 1
      case BlockIn => n
    }

    (s_separate(n + 1, c) ~> c_ns_properties(n + 1, c)).? <~ s_l_comments >> {
      case None =>  l_block_sequence(seq_spaces(n, c), None) | l_block_mapping(n, None)
      case Some(props) => l_block_sequence(seq_spaces(n, c), Some(props)) | l_block_mapping(n, Some(props))
    }
  }
  //YAML Character Stream
  //Documents
  //TODO Document Prefix
  val l_document_prefix	=	c_byte_order_mark.? <~ l_comment.*
  //Document Markers
  val c_directives_end =	"---"
  val c_document_end = "..."
  val l_document_suffix = c_document_end ~ s_l_comments
  val c_forbidden = "^".r ~ (c_directives_end | c_document_end ) ~ (b_char | s_white | "$".r)
  //Bare Documents
  val l_bare_document = s_l_block_node(-1, BlockIn)
  /* Excluding c-forbidden content */

  def parse(text : String): ParseResult[Any] = parse(l_bare_document, text)



/*

  val mapSeparator = ": \r?\n?"r
  val seqIndicator = "- ?\r?\n?"r
  val comment =  *#[^\r\n]*
  val commentLine = ("\n*" + comment + "\r*\n+").r
  def mappingKey = [^-:\{\[\r\n][^-:\r\n]*r
  val newLine = commentLine |  *\r*\n+.r
  val inlineSeparator = " *,[ \r\n]+".r
  val mapBegin = """\{[ \n\r]*"""r
  val mapEnd = """[\n ]*\}"""r
  val listBegin = """\[[ \n\r]*"""r
  val listEnd = """[\n ]*]"""r

  protected def leadingSpaces(numLeadingSpaces:Int): Parser[Int] =
    ("^[ ]{" + numLeadingSpaces + ",}"r) ^^ { _.length }

  def mappings(numLeadingSpaces: Int): Parser[Map[String, Any]] =
    indentedMap(numLeadingSpaces) | inlineMap(numLeadingSpaces)

  def listOfMapsFlatten(map: List[Map[String, Any]]): Map[String, Any] =
    map.foldRight(Map[String, Any]()) { (m, acc) =>
      m ++ acc
    }

  def indentedMap(numLeadingSpaces: Int): Parser[Map[String, Any]] =
    rep1sep(indentedMapping(numLeadingSpaces), newLine) ^^ listOfMapsFlatten

  def inlineMap(numLeadingSpaces: Int): Parser[Map[String, Any]] = mapBegin ~>
    repsep(inlineMapping(numLeadingSpaces), inlineSeparator) <~ mapEnd ^^ listOfMapsFlatten

  def indentedMapping(numLeadingSpaces: Int): Parser[Map[String, Any]] =
    leadingSpaces(numLeadingSpaces) ~ mappingKey >> { case i ~ key => (
      ": +".r ~> (inlineMap(i + 1) | inlineList(i + 1) |
        scalar( """[^\r\n#][^\r\n#]*""", i + 1)) |
        ":" ~> newLine ~> (indentedList(i + 1) | indentedMap(i + 1))
      ) ^^ { case value => Map(key -> value) } }

  def inlineMapping(numLeadingSpaces: Int): Parser[Map[String, Any]] =
    mappingKey ~ mapSeparator ~ (inlineList(numLeadingSpaces + 1) | inlineMap(numLeadingSpaces + 1) |
      scalar("""[^,\r\n\}#]+""", numLeadingSpaces + 1)) ^^ { case key ~_ ~ value => Map(key -> value) }

  def list(numLeadingSpaces:Int): Parser[List[Any]] =
    (inlineList(numLeadingSpaces) | indentedList(numLeadingSpaces)) ^^ { List() ++ _ }

  def indentedList(numLeadingSpaces:Int): Parser[List[Any]] =
    rep1sep( leadingSpaces(numLeadingSpaces) <~ seqIndicator >> { i => nestedListData(i + 1) }, newLine)

  def inlineList(numLeadingSpaces: Int): Parser[List[Any]] = listBegin ~> repsep(nestedListData(0), inlineSeparator) <~ listEnd

  def nestedListData(numLeadingSpaces: Int): Parser[Any] =
    list(numLeadingSpaces) | mappings(numLeadingSpaces) |
      (indentedMapping(0) ~ newLine ~ rep1sep(indentedMapping(numLeadingSpaces + 1) // + 1 from space after -
        , newLine) ^^ { case head ~ _ ~ tail => head ++ listOfMapsFlatten(tail)
      }) |
      scalar( """[^,\r\n\]\{\}\[#]+""", numLeadingSpaces + 1)

  def blockScalar(numLeadingSpaces: Int): Parser[String] =
    ("|" | ">") ~ """[^\r\n]*""".r ~ """\r*\n+""".r ~ repsep(leadingSpaces(numLeadingSpaces) ~> """[^\r\n]*""".r, """\r*\n+""".r) ^^ {
      case "|" ~ first ~ _ ~ l => l.foldLeft(first)((acc, s) => acc + "\n" + s)
      case ">" ~ first ~ _ ~ l => l.foldLeft(first)((acc, s) => acc + " " + s).trim
    }

  def inlineScalar(regexString: String) = regexString.r  ^^ { case value => value.trim }

  def scalar(regexString: String, numLeadingSpaces: Int): Parser[String] =  blockScalar(numLeadingSpaces) | inlineScalar(regexString)

  def yaml: Parser[Any] = namedError(opt(newLine) ~> (list(0) | mappings(0)), "Not YAML!")

  def parse(text : String): ParseResult[Any] = parse(yaml, text)


  def namedError[A](p: Parser[A], msg: String): Parser[A] = Parser[A] { i =>
    p(i) match {
      case Failure(_, in) => Failure(msg, in)
      case o => o
    }
  }
  */
}
