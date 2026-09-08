package io.taig.otter.codec

import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Metadata
import io.taig.otter.Side
import io.taig.otter.Typescript
import io.taig.otter.TypescriptKeys

/** Gives a named schema a declaration of its own, and breaks the cycle when one refers to itself.
  *
  * The fixpoint every TypeScript target shares. What varies is only [[JsonTypescriptTarget]] and the expression
  * renderer, both handed in; what does not vary is this:
  *
  *   - a schema with no name is rendered where it stands, and whatever its children hoisted comes along;
  *   - a name already being rendered is a cycle, so the reference is suspended and the definition that is open when it
  *     happens is the one that will need to declare its type by hand;
  *   - a name already declared is just a reference;
  *   - otherwise the body is rendered with the name in progress, and what it turned out to need decides whether the
  *     declaration infers its type from its value or is told it.
  *
  * `expression` is handed `this` rather than itself, so that a child reaching a named schema comes back through the
  * naming rules instead of inlining it. That knot is why the recursion terminates.
  *
  * `rename` is applied to every name, both where a definition is declared and everywhere it is referred to, so that two
  * runs of the same schema can be told apart by the names they declare without either of them having to be rewritten
  * afterwards.
  *
  * A name is the only thing that can stop a cycle, so a recursive schema has to carry [[io.taig.otter.Keys.name]] on
  * the value that is reached again -- the `lazy val` itself, not a wrapper around it. Naming the wrapper leaves the
  * inner reference anonymous, and a renderer that never meets a name it has already met does not terminate.
  */
final class JsonStateTypescriptRenderer(
    side: Side,
    namespaces: NonEmptyList[Metadata.Namespace],
    target: JsonTypescriptTarget,
    rename: String => String,
    expression: Renderer[Json.Node, State[JsonTypescriptContext, Typescript.Expression]] => Renderer[
      Json.Node,
      State[JsonTypescriptContext, Typescript.Expression]
    ]
) extends Renderer[Json.Node, State[JsonTypescriptContext, Typescript.Expression]]:
  private lazy val body: Renderer[Json.Node, State[JsonTypescriptContext, Typescript.Expression]] =
    JsonTypescriptExpressionOverrideRenderer(namespaces, expression(this))

  /** The structural type a recursive definition declares. A named child appears as its name, which is what keeps this
    * from following the same cycle the expression renderer just broke.
    */
  private lazy val structural: Renderer[Json.Node, Typescript.Type] = JsonTypescriptTypeOverrideRenderer(
    namespaces,
    new JsonTypescriptTypeRenderer(
      side,
      Renderer([w, r] =>
        (json: Json.Node[w, r]) =>
          name(json) match
            case Some(name) => Typescript.Type.Symbol(name, parameters = Nil)
            case None       => structural.render(json)
      )
    )
  )

  private def name(json: Json.Node[?, ?]): Option[String] = Json.name(namespaces, json).map(rename)

  override def render[W, R](json: Json.Node[W, R]): State[JsonTypescriptContext, Typescript.Expression] =
    State: context =>
      name(json) match
        case None       => body.render(json).run(context).value
        case Some(name) =>
          val symbol = Typescript.Expression.Symbol(name)

          if context.stack.contains(name) then (context.recursive(true), target.suspend(symbol))
          else if context.definitions.contains(name) then agreed(json, name, symbol, context)
          else
            val (update, definition) = definitionOf(json, name, symbol, context)

            (update.restore(context).updated(name, definition, side), symbol)

  /** The definition a name binds, and the context left once its body has been rendered. */
  private def definitionOf[W, R](
      json: Json.Node[W, R],
      name: String,
      symbol: Typescript.Expression.Symbol,
      context: JsonTypescriptContext
  ): (JsonTypescriptContext, JsonTypescriptDefinition) =
    val (update, expression) = body.render(json).run(context.push(name)).value

    /* A type the schema asked for wins, and a cycle forces one; anything else is inferred from the value. Declaring a
     * type at all is what makes the constant need an ascription, because inference would otherwise disagree with what
     * was just declared. */
    val declared = Json
      .attr(namespaces, Json.metadata(json), TypescriptKeys.tpe)
      .orElse(Option.when(update.recursive)(structural.render(json)))

    val definition = declared.fold(JsonTypescriptDefinition(target.inferred(symbol), none, expression)): tpe =>
      JsonTypescriptDefinition(tpe, target.annotation(name).some, expression)

    (update, definition)

  /** A name that is already declared, answered with the symbol -- and checked, where the declaration standing there was
    * rendered at the other [[Side]].
    *
    * Reaching a name a second time is what makes a shared schema declared once, and at one side the declaration is by
    * construction the one this reach would have produced. Across sides it may not be: the two sides of a schema differ
    * wherever a field is optional or holds a default, and an endpoint renders its request at one side and its response
    * at the other. Binding one name to the writer's declaration and then referring to it from a reader would describe a
    * value nothing produces, so the disagreement is rendered and compared rather than assumed away. Where the two agree
    * -- which is every schema with no such field, and most of them -- nothing is recorded and the declaration is shared
    * as before.
    *
    * The re-render is thrown away whatever it says. Its own nested definitions go with it, and so does the expression
    * that referred to them, so nothing is left dangling; what stays is the first side's declaration, and a conflict is
    * something the caller is told about rather than something this can resolve. Naming the two apart is the schema's
    * decision to make.
    */
  private def agreed[W, R](
      json: Json.Node[W, R],
      name: String,
      symbol: Typescript.Expression.Symbol,
      context: JsonTypescriptContext
  ): (JsonTypescriptContext, Typescript.Expression) =
    if context.sides.get(name).contains(side) then (context, symbol)
    else
      val (_, definition) = definitionOf(json, name, symbol, context)

      if context.definitions.get(name).contains(definition) then (context, symbol)
      else (context.conflicted(name), symbol)
