package io.taig.otter.http.codec

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.http.Parameter
import io.taig.otter.http.Path
import io.taig.otter.http.Segment

/** A path as it reads with no request in hand: the literals it spells, and the names standing where its values go.
  *
  * Shared because two callers need the same walk for opposite reasons. A renderer wants `/reports/{id}` and the
  * parameters to describe. A router wants to know whether an incoming path *is* this path before it decodes anything,
  * and that is not the question [[PathDecoder]] answers -- a tuple decoder rejects the wrong arity and a
  * [[io.taig.otter.Constant]] rejects a mis-spelled literal, so "some other endpoint" and "this endpoint, called
  * wrongly" come back as the same `Invalid`. Matching on the arity and the literals alone is what separates them, and
  * it is the only part of a path a router may look at before deciding the request is its own.
  */
object PathTemplate:
  /** Every segment, as either the literal it spells or the name and value it stands for. */
  def apply(schema: Path.Node[?, ?]): Chain[Either[String, (String, Parameter.Node[?, ?])]] =
    Path
      .segments(schema)
      .map:
        case Segment.Static.Schema(node)  => PathTemplate.literal(node.self).asLeft
        case Segment.Dynamic.Schema(node) => (node.self.name, node.self.schema.value: Parameter.Node[?, ?]).asRight

  /** The path, with a placeholder where every dynamic segment stands. */
  def render(schema: Path.Node[?, ?]): String =
    val pieces = PathTemplate(schema).map:
      case Left(literal)    => literal
      case Right((name, _)) => s"{$name}"

    "/" ++ pieces.toList.mkString("/")

  /** The dynamic segments, in the order they appear. */
  def placeholders(schema: Path.Node[?, ?]): Chain[(String, Parameter.Node[?, ?])] =
    PathTemplate(schema).collect { case Right(placeholder) => placeholder }

  /** The text a static segment spells, pushed back through the schema that writes it. */
  private def literal(schema: Self.Constant[Parameter.Primitive.Node, ?, ?]): String = schema match
    case Self.Constant.Modify(self, _, _)        => PathTemplate.literal(self)
    case Self.Constant.Root(reference, value, _) => ParameterPrimitiveEncoder.encode(reference.value, value.value)
