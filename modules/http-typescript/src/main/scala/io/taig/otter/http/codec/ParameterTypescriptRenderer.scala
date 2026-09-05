package io.taig.otter.http.codec

import cats.data.NonEmptyList
import io.taig.otter as Self
import io.taig.otter.Typescript
import io.taig.otter.codec.PrimitiveTypescriptTypeLiteralEncoder
import io.taig.otter.codec.Renderer
import io.taig.otter.http.Parameter

/** The TypeScript type of what a path segment, a query parameter or a header holds.
  *
  * A parameter's schema describes the *value* and not the text, which is OpenAPI's convention and is the one that makes
  * a generated client usable: a caller holds a `number` and the descriptor is what knows it goes on the wire as one.
  * [[ParameterTypescriptRenderer.text]] is the other half, and the two are written together so that neither can drift
  * into describing a value the other does not write.
  *
  * A [[Parameter.Coerce]] renders as its canonical form. The laxer spellings a coercion accepts are read and never
  * written, so a caller has nothing to choose between.
  */
object ParameterTypescriptRenderer extends Renderer[Parameter.Node, Typescript.Type]:
  val Boolean: Typescript.Type = Typescript.Type.Symbol("boolean", parameters = Nil)
  val Number: Typescript.Type = Typescript.Type.Symbol("number", parameters = Nil)
  val Text: Typescript.Type = Typescript.Type.Symbol("string", parameters = Nil)

  override def render[W, R](parameter: Parameter.Node[W, R]): Typescript.Type = parameter match
    case Parameter.Collection.Schema(node) =>
      Typescript.Type.Symbol("ReadonlyArray", List(collection(node.self)))
    case Parameter.Coerce.Schema(node)         => coerce(node.self)
    case Parameter.Constant.Schema(node)       => constant(node.self)
    case Parameter.Enumeration.Schema(node)    => Typescript.Type.Union(enumeration(node.self))
    case Parameter.Primitive.Boolean.Schema(_) => ParameterTypescriptRenderer.Boolean
    case Parameter.Primitive.Number.Schema(_)  => ParameterTypescriptRenderer.Number
    case Parameter.Primitive.Text.Schema(_)    => ParameterTypescriptRenderer.Text

  /** Whether the values a parameter holds are already text, and so need no conversion on the way out.
    *
    * Asked rather than assumed because `String(value)` around something that is already a string is noise in generated
    * source, and generated source is read.
    */
  def isText(parameter: Parameter.Node[?, ?]): Boolean = parameter match
    case Parameter.Collection.Schema(node)     => isText(element(node.self))
    case Parameter.Coerce.Schema(node)         => isText(canonical(node.self))
    case Parameter.Constant.Schema(node)       => isText(constant(node.self))
    case Parameter.Enumeration.Schema(node)    => enumeration(node.self).forall(isText)
    case Parameter.Primitive.Boolean.Schema(_) => false
    case Parameter.Primitive.Number.Schema(_)  => false
    case Parameter.Primitive.Text.Schema(_)    => true

  /** The value a parameter holds, as the text it goes on the wire as.
    *
    * A repeated parameter is spelled element by element rather than as a whole. Every parameter is text on the wire,
    * and that is as true of one given many times as of one given once -- handing a caller an array of numbers where a
    * scalar would have been converted would be the same claim made two different ways.
    */
  def text(parameter: Parameter.Node[?, ?], value: Typescript.Expression): Typescript.Expression =
    if isText(parameter) then value
    else if isRepeated(parameter) then
      Typescript.Expression.Invoke(value, "map", List(Typescript.Expression.Symbol("String")))
    else Typescript.Expression.Call("String", List(value))

  /** Whether a parameter is given more than once, which is what decides whether its wire form is one value or many. */
  def isRepeated(parameter: Parameter.Node[?, ?]): Boolean = parameter match
    case Parameter.Collection.Schema(_) => true
    case _                              => false

  /** The element of a repeated parameter, or the parameter itself when it is given once. */
  def element(parameter: Parameter.Node[?, ?]): Parameter.Node[?, ?] = parameter match
    case Parameter.Collection.Schema(node) => element(node.self)
    case parameter                         => parameter

  /** Whether a literal is spelled with quotes, which is the whole of what makes it text already. */
  private def isText(literal: Typescript.Type.Literal): Boolean = literal match
    case Typescript.Type.Literal.String(_) => true
    case _                                 => false

  private def collection(schema: Self.Collection[Parameter.Value.Node, ?, ?]): Typescript.Type =
    render(element(schema))

  private def element(schema: Self.Collection[Parameter.Value.Node, ?, ?]): Parameter.Value.Node[?, ?] = schema match
    case Self.Collection.Modify(self, _, _)    => element(self)
    case Self.Collection.Chained(reference, _) => reference.value
    case Self.Collection.Indexed(reference, _) => reference.value
    case Self.Collection.Linked(reference, _)  => reference.value

  private def coerce(schema: Self.Coerce[Parameter.Primitive.Node, ?, ?]): Typescript.Type = render(canonical(schema))

  private def canonical(schema: Self.Coerce[Parameter.Primitive.Node, ?, ?]): Parameter.Primitive.Node[?, ?] =
    schema match
      case Self.Coerce.Modify(self, _, _) => canonical(self)
      case Self.Coerce.Root(reference)    => reference.value

  private def constant(schema: Self.Constant[Parameter.Primitive.Node, ?, ?]): Typescript.Type.Literal = schema match
    case Self.Constant.Modify(self, _, _)        => constant(self)
    case Self.Constant.Root(reference, value, _) => literal(reference.value, value.value)

  private def enumeration(
      schema: Self.Enumeration[Parameter.Primitive.Node, ?, ?]
  ): NonEmptyList[Typescript.Type.Literal] = schema match
    case Self.Enumeration.Modify(self, _, _)       => enumeration(self)
    case Self.Enumeration.Root(reference, mapping) =>
      mapping.values.map(value => literal(reference.value, mapping.inj(value)))

  /** The value, pushed back through the very schema that describes it -- which is what [[JsonTypescriptLiteral]] does
    * one alphabet over, and for the same reason: what a literal looks like is the schema's word and not the Scala
    * type's.
    */
  private def literal[W](schema: Parameter.Primitive.Node[W, Any], value: W): Typescript.Type.Literal = schema match
    case Parameter.Primitive.Boolean.Schema(node) => PrimitiveTypescriptTypeLiteralEncoder.encode(node.self, value)
    case Parameter.Primitive.Number.Schema(node)  => PrimitiveTypescriptTypeLiteralEncoder.encode(node.self, value)
    case Parameter.Primitive.Text.Schema(node)    => PrimitiveTypescriptTypeLiteralEncoder.encode(node.self, value)
