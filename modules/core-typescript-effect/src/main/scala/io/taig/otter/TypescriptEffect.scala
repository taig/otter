package io.taig.otter

import cats.data.NonEmptyList

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import scala.Boolean as SBoolean

/** The vocabulary of the Effect v4 `Schema` module, as [[Typescript]].
  *
  * Everything a renderer needs to say is spelled here once, so that a renderer reads as a translation of the schema and
  * not as a string template. Nothing in this object knows about a schema; it is the target language, not the source.
  */
object TypescriptEffect:
  /** The [[Metadata.Namespace]] the effect renderers read their attributes from, whatever the format being rendered. */
  val Namespace: Metadata.Namespace = Metadata.Namespace("typescript-effect")

  val Module: JString = "effect"

  private val Modules: List[JString] = List("Schema", "SchemaTransformation", "SchemaGetter", "Option")

  /** Import and global names referenced by generated schemas and HTTP descriptors. */
  val Reserved: Set[JString] =
    Modules.toSet ++ Set("String", "Number", "RegExp", "ReadonlyArray", "Record", "Blob", "Date")

  /** The Effect imports referenced by a generated program, in a stable order. Expression overrides may reference these
    * namespaces too; imports from other libraries remain the caller's responsibility.
    */
  def imports(source: List[Typescript]): List[Typescript.Statement.Import] =
    def all(values: Iterable[Typescript]): Set[JString] = values.iterator.flatMap(namespaces).toSet
    def namespaces(value: Typescript): Set[JString] = value match
      case Typescript.Expression.Member(namespace, property)        => namespaces(property) + namespace
      case Typescript.Type.Member(namespace, property)              => namespaces(property) + namespace
      case Typescript.Expression.Array(elements)                    => all(elements)
      case Typescript.Expression.Arrow(arguments, body)             => all(arguments) ++ namespaces(body)
      case Typescript.Expression.AsConst(self)                      => namespaces(self)
      case Typescript.Expression.Call(_, arguments)                 => all(arguments)
      case Typescript.Expression.Binary(left, _, right)             => all(List(left, right))
      case Typescript.Expression.Equal(left, right)                 => all(List(left, right))
      case Typescript.Expression.Function(parameters, body)         => all(parameters.map(_._2)) ++ namespaces(body)
      case Typescript.Expression.Index(self, index)                 => all(List(self, index))
      case Typescript.Expression.Invoke(self, _, arguments)         => namespaces(self) ++ all(arguments)
      case Typescript.Expression.Object(fields)                     => all(fields.map(_._2))
      case Typescript.Expression.Pipe(self, arguments)              => namespaces(self) ++ all(arguments.toList)
      case Typescript.Expression.Ternary(condition, valid, invalid) => all(List(condition, valid, invalid))
      case Typescript.Expression.TripleEqual(left, right)           => all(List(left, right))
      case _: Typescript.Expression.Literal | _: Typescript.Expression.Symbol | Typescript.Expression.Null |
          Typescript.Expression.Undefined =>
        Set.empty
      case Typescript.Statement.Block(statements)                      => all(statements)
      case Typescript.Statement.Declaration.Constant(_, _, tpe, value) => all(tpe.toList) ++ namespaces(value)
      case Typescript.Statement.Declaration.Variable(_, _, tpe, value) => all(tpe.toList) ++ namespaces(value)
      case Typescript.Statement.Declaration.Type(_, _, tpe)            => namespaces(tpe)
      case Typescript.Statement.Return(expression)                     => namespaces(expression)
      case Typescript.Statement.Evaluate(expression)                   => namespaces(expression)
      case _: Typescript.Statement.Import                              => Set.empty
      case Typescript.Type.Function(parameters, result) => all(parameters.map(_._2)) ++ namespaces(result)
      case Typescript.Type.Object(fields)               => all(fields.map(_.tpe))
      case Typescript.Type.Symbol(_, parameters)        => all(parameters)
      case Typescript.Type.Readonly(self)               => namespaces(self)
      case Typescript.Type.Rest(self)                   => namespaces(self)
      case Typescript.Type.Tuple(elements)              => all(elements)
      case Typescript.Type.TypeOf(expression)           => namespaces(expression)
      case Typescript.Type.Union(types)                 => all(types.toList)
      case _: Typescript.Type.Literal | Typescript.Type.Null | Typescript.Type.Undefined => Set.empty

    val used = all(source)
    NonEmptyList
      .fromList(Modules.filter(used.contains))
      .toList
      .map(Typescript.Statement.Import(_, TypescriptEffect.Module))

  /** `Schema.<expression>`. */
  def apply(expression: Typescript.Expression): Typescript.Expression =
    Typescript.Expression.Member(namespace = "Schema", expression)

  /** `Schema.<tpe>`. */
  def apply(tpe: Typescript.Type): Typescript.Type = Typescript.Type.Member(namespace = "Schema", tpe)

  /** `Schema.<name>`, a member of the module named directly. */
  def symbol(name: JString): Typescript.Expression = apply(Typescript.Expression.Symbol(name))

  private def call(name: JString, arguments: Typescript.Expression*): Typescript.Expression =
    moduleCall("Schema", name, arguments*)

  private def moduleCall(namespace: JString, name: JString, arguments: Typescript.Expression*): Typescript.Expression =
    Typescript.Expression.Member(namespace, Typescript.Expression.Call(name, arguments.toList))

  val Boolean: Typescript.Expression = symbol("Boolean")

  /** `Schema.isInt()`, the check that says a number is a safe integer. */
  val Integral: Typescript.Expression = apply(Typescript.Expression.Call("isInt", Nil))
  val Int: Typescript.Expression = symbol("Int")

  /** Effect's int filter requires a safe integer; Long also admits other representable integral numbers. */
  val IntegralNumber: Typescript.Expression = filter(
    "makeFilter",
    Typescript.Expression.Member("Number", Typescript.Expression.Symbol("isInteger"))
  )
  val Null: Typescript.Expression = symbol("Null")
  val Number: Typescript.Expression = symbol("Number")
  val String: Typescript.Expression = symbol("String")

  def array(element: Typescript.Expression): Typescript.Expression = call("Array", element)

  /** An array that always holds at least one element, which effect types as a tuple with a rest rather than as a list.
    */
  def nonEmptyArray(element: Typescript.Expression): Typescript.Expression = call("NonEmptyArray", element)

  def literal(values: NonEmptyList[Typescript.Expression.Literal]): Typescript.Expression = values match
    case NonEmptyList(value, Nil) => call("Literal", value)
    case values                   => call("Literals", Typescript.Expression.Array(values.toList))

  def nullOr(self: Typescript.Expression): Typescript.Expression = call("NullOr", self)

  /** A field that may be absent by having no key at all. */
  def optional(self: Typescript.Expression): Typescript.Expression = call("optional", self)

  /** A field that may be absent by having no key or by holding a `null`, which is what a lenient field reads. */
  def optionalNullable(self: Typescript.Expression): Typescript.Expression =
    val present = arrow(
      Typescript.Expression.Binary(Value, Typescript.Expression.Operator.StrictNotEqual, Typescript.Expression.Null)
    )
    val transformation = Typescript.Expression.Object(
      List(
        "decode" -> moduleCall("SchemaGetter", "transformOptional", moduleCall("Option", "filter", present)),
        "encode" -> moduleCall("SchemaGetter", "transformOptional", arrow(Value))
      )
    )
    Typescript.Expression.Pipe(
      optional(nullOr(self)),
      NonEmptyList.one(call("decodeTo", optional(call("toType", self)), transformation))
    )

  def record(value: Typescript.Expression): Typescript.Expression = call(
    "Record",
    TypescriptEffect.String,
    value
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
    val transformation = moduleCall(
      "SchemaTransformation",
      "transform",
      Typescript.Expression.Object(List("decode" -> decode, "encode" -> encode))
    )
    Typescript.Expression.Pipe(from, NonEmptyList.one(call("decodeTo", to, transformation)))

  def tuple(elements: List[Typescript.Expression]): Typescript.Expression =
    call("Tuple", Typescript.Expression.Array(elements))

  def union(members: NonEmptyList[Typescript.Expression]): Typescript.Expression = members match
    case NonEmptyList(member, Nil) => member
    case members                   => call("Union", Typescript.Expression.Array(members.toList))

  /** `self.check(a, b)`, which is how checks narrow an already built schema. */
  def filtered(self: Typescript.Expression, filters: List[Typescript.Expression]): Typescript.Expression =
    if filters.isEmpty then self else Typescript.Expression.Invoke(self, "check", filters)

  /** `Schema.Schema.Type<tpe>`, the type a non recursive definition infers from its own value. */
  def inferred(tpe: Typescript.Type): Typescript.Type = apply(apply(Typescript.Type.Symbol("Type", List(tpe))))

  /** `Schema.Codec.Encoded<tpe>`, what a value looks like before it is decoded.
    *
    * The counterpart of [[TypescriptEffect.inferred]], and not a symmetry for its own sake: the encoded side is the one
    * that is still made of the things JSON has, so it is the side that survives being serialised, cached and handed
    * back. A caller who has to hold a response across such a boundary has to be able to name it.
    */
  def encoded(tpe: Typescript.Type): Typescript.Type =
    apply(Typescript.Type.Member("Codec", Typescript.Type.Symbol("Encoded", List(tpe))))

  /** `Schema.Codec<tpe, tpe>`, preserving both projections and the absence of services across a recursive cycle.
    */
  def annotation(tpe: Typescript.Type): Typescript.Type = annotation(tpe, tpe)

  def annotation(decoded: Typescript.Type, encoded: Typescript.Type): Typescript.Type =
    apply(Typescript.Type.Symbol("Codec", List(decoded, encoded)))

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

  private val NumericText: Typescript.Expression = filtered(
    String,
    List(
      filter(
        "isPattern",
        Typescript.Expression.Call(
          "RegExp",
          List(
            Typescript.Expression.Literal.String(
              "^-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?(?![\\s\\S])"
            )
          )
        )
      )
    )
  )

  private def numericText(from: Typescript.Expression): Typescript.Expression = transform(
    from,
    Number,
    arrow(Typescript.Expression.Call("Number", List(Value))),
    arrow(Typescript.Expression.Call("String", List(Value)))
  )

  val CoerceNumber: Typescript.Expression = union(NonEmptyList.of(Number, numericText(NumericText)))

  val CoerceInt: Typescript.Expression = union(
    NonEmptyList.of(
      Number,
      numericText(
        filtered(NumericText, List(filter("makeFilter", io.taig.otter.codec.TypescriptIntString.predicate)))
      )
    )
  )

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
        decode = arrow(Typescript.Expression.Call("String", List(Value))),
        encode = arrow(Typescript.Expression.TripleEqual(Value, True))
      )
    )
  )

  /** A check, as the `Schema.<name>(<reference>)` that [[filtered]] applies. */
  private[otter] def filter(name: JString, reference: Typescript.Expression): Typescript.Expression =
    call(name, reference)

  private[otter] def number(value: JBigDecimal): Typescript.Expression =
    Typescript.Expression.Literal.Number(value)
