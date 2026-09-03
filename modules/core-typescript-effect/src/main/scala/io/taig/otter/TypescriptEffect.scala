package io.taig.otter

import cats.data.NonEmptyList

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import scala.Boolean as SBoolean

/** The vocabulary of the `effect` `Schema` module, as [[Typescript]].
  *
  * Everything a renderer needs to say is spelled here once, so that a renderer reads as a translation of the schema and
  * not as a string template. Nothing in this object knows about a schema; it is the target language, not the source.
  */
object TypescriptEffect:
  /** The [[Metadata.Namespace]] the effect renderers read their attributes from, whatever the format being rendered. */
  val Namespace: Metadata.Namespace = Metadata.Namespace("typescript-effect")

  /** `Schema.<expression>`. */
  def apply(expression: Typescript.Expression): Typescript.Expression =
    Typescript.Expression.Member(namespace = "Schema", expression)

  /** `Schema.<tpe>`. */
  def apply(tpe: Typescript.Type): Typescript.Type = Typescript.Type.Member(namespace = "Schema", tpe)

  /** `Schema.<name>`, a member of the module named directly. */
  def symbol(name: JString): Typescript.Expression = apply(Typescript.Expression.Symbol(name))

  private def call(name: JString, arguments: Typescript.Expression*): Typescript.Expression =
    apply(Typescript.Expression.Call(name, arguments.toList))

  val Boolean: Typescript.Expression = symbol("Boolean")

  /** `Schema.int()`, the filter that says a number carries no fraction. */
  val Integral: Typescript.Expression = apply(Typescript.Expression.Call("int", Nil))
  val Int: Typescript.Expression = symbol("Int")
  val Null: Typescript.Expression = symbol("Null")
  val Number: Typescript.Expression = symbol("Number")
  val String: Typescript.Expression = symbol("String")

  def array(element: Typescript.Expression): Typescript.Expression = call("Array", element)

  /** An array that always holds at least one element, which effect types as a tuple with a rest rather than as a list.
    */
  def nonEmptyArray(element: Typescript.Expression): Typescript.Expression = call("NonEmptyArray", element)

  def literal(values: NonEmptyList[Typescript.Expression.Literal]): Typescript.Expression =
    call("Literal", values.toList*)

  def nullOr(self: Typescript.Expression): Typescript.Expression = call("NullOr", self)

  /** A field that may be absent by having no key at all. */
  def optional(self: Typescript.Expression): Typescript.Expression = call("optional", self)

  /** A field that may be absent by having no key or by holding a `null`, which is what a lenient field reads. */
  def optionalNullable(self: Typescript.Expression): Typescript.Expression = call(
    "optionalWith",
    self,
    Typescript.Expression.Object(List("nullable" -> Typescript.Expression.Literal.Boolean(true)))
  )

  def record(value: Typescript.Expression): Typescript.Expression = call(
    "Record",
    Typescript.Expression.Object(List("key" -> TypescriptEffect.String, "value" -> value))
  )

  def struct(fields: List[(JString, Typescript.Expression)]): Typescript.Expression =
    call("Struct", Typescript.Expression.Object(fields))

  /** `Schema.suspend(() => self)`, which is how a definition refers to itself before it exists. */
  def suspend(self: Typescript.Expression): Typescript.Expression =
    call("suspend", Typescript.Expression.Arrow(arguments = Nil, body = self))

  def transform(
      from: Typescript.Expression,
      to: Typescript.Expression,
      decode: Typescript.Expression,
      encode: Typescript.Expression
  ): Typescript.Expression =
    call("transform", from, to, Typescript.Expression.Object(List("decode" -> decode, "encode" -> encode)))

  def tuple(elements: List[Typescript.Expression]): Typescript.Expression = call("Tuple", elements*)

  def union(members: NonEmptyList[Typescript.Expression]): Typescript.Expression = members match
    case NonEmptyList(member, Nil) => member
    case members                   => call("Union", members.toList*)

  /** `self.pipe(a, b)`, which is how a filter narrows an already built schema. */
  def filtered(self: Typescript.Expression, filters: List[Typescript.Expression]): Typescript.Expression =
    NonEmptyList.fromList(filters).fold(self)(Typescript.Expression.Pipe(self, _))

  /** `Schema.Schema.Type<tpe>`, the type a non recursive definition infers from its own value. */
  def inferred(tpe: Typescript.Type): Typescript.Type = apply(apply(Typescript.Type.Symbol("Type", List(tpe))))

  /** `Schema.Schema<tpe>`, the annotation a recursive definition needs because inference cannot see through the cycle.
    */
  def annotation(tpe: Typescript.Type): Typescript.Type = apply(Typescript.Type.Symbol("Schema", List(tpe)))

  /** Whether a declared type was inferred from its value, which is what decides if the constant needs an annotation. */
  def isInferred(tpe: Typescript.Type): SBoolean = tpe match
    case Typescript.Type.Member("Schema", Typescript.Type.Member("Schema", Typescript.Type.Symbol("Type", _))) => true
    case _                                                                                                     => false

  private val Value: Typescript.Expression = Typescript.Expression.Symbol("value")

  private def arrow(body: Typescript.Expression): Typescript.Expression =
    Typescript.Expression.Arrow(arguments = List(Value), body = body)

  private val True: Typescript.Expression.Literal = Typescript.Expression.Literal.String("true")

  private val False: Typescript.Expression.Literal = Typescript.Expression.Literal.String("false")

  /** Text that reads as the boolean it spells and writes back as that spelling. */
  private val BooleanFromString: Typescript.Expression = transform(
    from = union(NonEmptyList.of(literal(NonEmptyList.one(True)), literal(NonEmptyList.one(False)))),
    to = TypescriptEffect.Boolean,
    decode = arrow(Typescript.Expression.TripleEqual(Value, True)),
    encode = arrow(Typescript.Expression.Ternary(Value, True, False))
  )

  /** The laxer wire forms a [[Coerce]]d boolean, number and text accept, matching what the decoder normalises. */
  val CoerceBoolean: Typescript.Expression = union(NonEmptyList.of(TypescriptEffect.Boolean, BooleanFromString))

  val CoerceNumber: Typescript.Expression =
    union(NonEmptyList.of(TypescriptEffect.Number, symbol("NumberFromString")))

  val CoerceString: Typescript.Expression = union(
    NonEmptyList.of(
      TypescriptEffect.String,
      transform(
        from = TypescriptEffect.Number,
        to = TypescriptEffect.String,
        decode = arrow(Typescript.Expression.Call("String", List(Value))),
        encode = arrow(Typescript.Expression.Call("Number", List(Value)))
      ),
      transform(
        from = TypescriptEffect.Boolean,
        to = TypescriptEffect.String,
        decode = arrow(Typescript.Expression.Ternary(Value, True, False)),
        encode = arrow(Typescript.Expression.TripleEqual(Value, True))
      )
    )
  )

  /** A filter, as the `Schema.<name>(<reference>)` a [[filtered]] pipe takes. */
  private[otter] def filter(name: JString, reference: Typescript.Expression): Typescript.Expression =
    call(name, reference)

  private[otter] def number(value: JBigDecimal): Typescript.Expression =
    Typescript.Expression.Literal.Number(value)
