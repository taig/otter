package io.taig.otter.codec

import cats.data.Chain
import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Coerce
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.Optional
import io.taig.otter.Primitive
import io.taig.otter.Side
import io.taig.otter.Tuple
import io.taig.otter.Typescript
import io.taig.otter.TypescriptEffect

/** Describes one side of a JSON schema as an `effect` `Schema` value.
  *
  * The same dispatch on the alphabet the circe interpreters do, with two differences. No value takes part, so the side
  * has to be given rather than implied. And a name may have to be hoisted out to a declaration of its own, which is why
  * every child goes through `renderer` -- the fixpoint -- and why the result is a `State` rather than an expression.
  */
final class JsonTypescriptExpressionEffectRenderer(
    side: Side,
    renderer: Renderer[Json.Node, State[JsonTypescriptContext, Typescript.Expression]]
) extends Renderer[Json.Node, State[JsonTypescriptContext, Typescript.Expression]]:
  override def render[W, R](json: Json.Node[W, R]): State[JsonTypescriptContext, Typescript.Expression] = json match
    case Json.Coerce.Schema(node)     => coerce(node.self)
    case Json.Collection.Schema(node) =>
      val constraints = JsonTypescriptCollection.constraints(node.self)

      /* A minimum of one is the array effect has its own name for, and taking that name is what keeps the element's
       * presence in the type rather than only in the validation. */
      val rest = TypescriptConstraint.nonEmpty(constraints)

      val array: Typescript.Expression => Typescript.Expression =
        if rest.isDefined then TypescriptEffect.nonEmptyArray else TypescriptEffect.array

      child(node.self.schema.value)
        .map(array)
        .map(TypescriptEffect.filtered(_, ConstraintTypescriptEffect.filters(rest.getOrElse(constraints))))
    case Json.Constant.Schema(node) =>
      TypescriptEffect.literal(NonEmptyList.one(JsonTypescriptLiteral.constant(node.self))).pure
    case Json.Dictionary.Schema(node)     => child(node.self.schema.value).map(TypescriptEffect.record)
    case Json.Enumeration.Schema(node)    => TypescriptEffect.literal(JsonTypescriptLiteral.enumeration(node.self)).pure
    case Json.Optional.Schema(node)       => optional(node.self)
    case Json.Primitive.Boolean.Schema(_) => TypescriptEffect.Boolean.pure
    case Json.Primitive.Number.Schema(annotation) => number(annotation.self).pure
    case Json.Primitive.Text.Schema(annotation)   => text(annotation.self).pure
    case Json.Record.Schema(node)                 =>
      node.self.fields.toList.traverse(reference => field(reference.value)).map(TypescriptEffect.struct)
    case Json.Tuple.Schema(node) => tuple(node.self)
    case Json.Union.Schema(node) =>
      node.self.branches.toNonEmptyList
        .traverse(reference => child(reference.value.self.self.schema.value))
        .map(TypescriptEffect.union)

  private def child(json: Json.Node[?, ?]): State[JsonTypescriptContext, Typescript.Expression] = renderer.render(json)

  /** A record's member, whose key may be absent and whose value may be empty, depending on the side. */
  private def field(json: Json.Field.Node[?, ?]): State[JsonTypescriptContext, (String, Typescript.Expression)] =
    child(json.self.self.schema.value).map: expression =>
      val presence = Json.presence(side, json.self.metadata, json.self.self) match
        case Json.Presence.Required         => expression
        case Json.Presence.Nullable         => TypescriptEffect.nullOr(expression)
        case Json.Presence.Optional         => TypescriptEffect.optional(expression)
        case Json.Presence.OptionalNullable => TypescriptEffect.optionalNullable(expression)

      json.self.self.name -> presence

  /** The laxer wire forms the decoder normalises before handing over, which only the read side sees.
    *
    * The union of forms says nothing about the primitive underneath, so whatever that primitive was constrained by is
    * piped onto the result: the coercion decides what arrives, the filters still decide what is acceptable.
    */
  private def coerce[W, R](
      schema: Coerce[Json.Primitive.Node, W, R]
  ): State[JsonTypescriptContext, Typescript.Expression] =
    schema match
      case Coerce.Modify(self, _, _) => coerce(self)
      case Coerce.Root(reference)    =>
        side match
          case Side.Write => child(reference.value)
          case Side.Read  =>
            val (name, expression) = reference.value match
              case Json.Primitive.Boolean.Schema(_) => ("CoerceBoolean", TypescriptEffect.CoerceBoolean)
              case Json.Primitive.Number.Schema(_)  => ("CoerceNumber", TypescriptEffect.CoerceNumber)
              case Json.Primitive.Text.Schema(_)    => ("CoerceString", TypescriptEffect.CoerceString)

            hoist(name, expression).map(TypescriptEffect.filtered(_, filters(reference.value)))

  /** Declares a value the target needs but no schema names, and refers to it. */
  private def hoist(
      name: String,
      expression: Typescript.Expression
  ): State[JsonTypescriptContext, Typescript.Expression] =
    State: context =>
      val symbol = Typescript.Expression.Symbol(name)
      val definition = JsonTypescriptDefinition(
        tpe = TypescriptEffect.inferred(Typescript.Type.TypeOf(symbol)),
        annotation = none,
        expression = expression
      )

      (context.updated(name, definition), symbol)

  private def optional[W, R](schema: Optional[Json.Node, W, R]): State[JsonTypescriptContext, Typescript.Expression] =
    schema match
      case Optional.Modify(self, _, _)    => optional(self)
      case Optional.Root(reference)       => child(reference.value).map(TypescriptEffect.nullOr)
      case Optional.Default(reference, _) =>
        side match
          case Side.Write => child(reference.value)
          case Side.Read  => child(reference.value).map(TypescriptEffect.nullOr)

  private def tuple[W, R](schema: Tuple[Json.Node, W, R]): State[JsonTypescriptContext, Typescript.Expression] =
    schema.schemas.toList
      .traverse(reference => child(reference.value))
      .map: elements =>
        val self = TypescriptEffect.tuple(elements)

        if Json.absent(side, schema)
        then
          TypescriptEffect.union(
            NonEmptyList.of(self, TypescriptEffect.tuple(elements.map(_ => TypescriptEffect.Null)))
          )
        else self

  private def number[W, R](schema: Primitive.Number[W, R]): Typescript.Expression =
    TypescriptEffect.filtered(numeric(schema), ConstraintTypescriptEffect.filters(number(schema, Chain.empty)))

  /** An integral number is a number the decoder refuses when it has a fraction, which is a filter and not a type. */
  private def numeric[W, R](schema: Primitive.Number[W, R]): Typescript.Expression = schema match
    case Primitive.Number.BigDecimal(_)      => TypescriptEffect.Number
    case Primitive.Number.BigInteger(_)      => TypescriptEffect.Int
    case Primitive.Number.Double(_)          => TypescriptEffect.Number
    case Primitive.Number.Float(_)           => TypescriptEffect.Number
    case Primitive.Number.Int(_)             => TypescriptEffect.Int
    case Primitive.Number.Long(_)            => TypescriptEffect.Int
    case Primitive.Number.Modify(self, _, _) => numeric(self)

  private def number[W, R](schema: Primitive.Number[W, R], constraints: Chain[Constraint]): Chain[Constraint] =
    schema match
      case Primitive.Number.BigDecimal(validation) => constraints ++ validation.constraints
      case Primitive.Number.BigInteger(validation) => constraints ++ validation.constraints
      case Primitive.Number.Double(validation)     => constraints ++ validation.constraints
      case Primitive.Number.Float(validation)      => constraints ++ validation.constraints
      case Primitive.Number.Int(validation)        => constraints ++ validation.constraints
      case Primitive.Number.Long(validation)       => constraints ++ validation.constraints
      case Primitive.Number.Modify(self, _, _)     => number(self, constraints)

  /** Every text is a string on the wire, whatever it parses into, so a format narrows nothing here. What it does narrow
    * is what a caller can say through [[io.taig.otter.TypescriptKeys.expression]].
    */
  private def text[W, R](schema: Primitive.Text[W, R]): Typescript.Expression =
    TypescriptEffect.filtered(TypescriptEffect.String, ConstraintTypescriptEffect.filters(text(schema, Chain.empty)))

  private def text[W, R](schema: Primitive.Text[W, R], constraints: Chain[Constraint]): Chain[Constraint] =
    schema match
      case Primitive.Text.Root(validation)   => constraints ++ validation.constraints
      case Primitive.Text.Format(_, _, _)    => constraints
      case Primitive.Text.Modify(self, _, _) => text(self, constraints)

  /** Everything a coercion has to keep saying about the primitive it wraps.
    *
    * Integrality is not a constraint the primitive carries; it is which of the number nodes it is. On its own the node
    * says it by being [[TypescriptEffect.Int]], but a coercion has already replaced the node with a union of the forms
    * it accepts, so the same claim has to come back as a filter.
    */
  private def filters(json: Json.Primitive.Node[?, ?]): List[Typescript.Expression] = json match
    case Json.Primitive.Boolean.Schema(_)       => Nil
    case Json.Primitive.Text.Schema(annotation) =>
      ConstraintTypescriptEffect.filters(text(annotation.self, Chain.empty))
    case Json.Primitive.Number.Schema(annotation) =>
      val integral =
        if numeric(annotation.self) == TypescriptEffect.Int then List(TypescriptEffect.Integral) else Nil

      integral ++ ConstraintTypescriptEffect.filters(number(annotation.self, Chain.empty))
