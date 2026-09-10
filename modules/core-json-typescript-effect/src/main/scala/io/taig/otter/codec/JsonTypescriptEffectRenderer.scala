package io.taig.otter.codec

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.JsonTypescriptEffect
import io.taig.otter.Side
import io.taig.otter.Typescript

import scala.annotation.tailrec

/** Turns a JSON schema into `effect` `Schema` source.
  *
  * [[reader]] and [[writer]] each describe one side and end with the expression the schema itself came to, preceded by
  * whatever it had to declare along the way. [[module]] describes both sides at once, which is what a client wants: it
  * has to know what it may send as well as what it must be ready to receive.
  *
  * Nothing here emits the `import` the declarations need; the caller writes that line, because only the caller knows
  * where the module is going.
  *
  * A schema that refers to itself must carry [[io.taig.otter.Keys.name]] on the `lazy val` that is reached again, and
  * not on a wrapper around it. Names are allocated to schema instances, so separate schemas requesting the same name
  * receive distinct declarations. Names are normalized to portable identifiers and cannot shadow imports or globals
  * referenced by generated code.
  */
object JsonTypescriptEffectRenderer:
  /** What to call the two sides of a schema.
    *
    * [[Naming.Collapsed]] gives a definition one name whenever both sides agree on it, and only splits the ones that
    * genuinely differ, which keeps a symmetric schema reading as one type. The cost is that the name of a type then
    * depends on whether the schema happens to be symmetric, so making one field nullable can rename it.
    * [[Naming.Suffixed]] always splits, which says less but never moves.
    */
  enum Naming:
    case Collapsed, Suffixed

  /** What a document read under this schema looks like, and the declarations it needs. */
  val reader: Renderer[Json.Node, List[Typescript]] = rendered(Side.Read)

  /** What a document written under this schema looks like, and the declarations it needs. */
  val writer: Renderer[Json.Node, List[Typescript]] = rendered(Side.Write)

  /** Both sides of every schema, as the declarations a module is made of.
    *
    * Only a named schema has a declaration to contribute; an anonymous one has nowhere to go and is left out, so name
    * the ones that matter with [[io.taig.otter.Keys.name]].
    */
  def module(schemas: Json.Node[?, ?]*): List[Typescript.Statement.Declaration] =
    module(Naming.Collapsed, schemas*)

  /** Every named schema on one side, under the name it was given.
    *
    * For a consumer that only ever meets one of the two shapes: a client reading what a server writes describes the
    * write side and nothing else, and would rather call the type `Place` than `PlaceWrite`. There is no [[Naming]] to
    * make, because there is no second side to tell the first one apart from.
    */
  def module(side: Side, schemas: Json.Node[?, ?]*): List[Typescript.Statement.Declaration] =
    definitions(side, JsonTypescriptEffect.Context, schemas).declarations

  def module(naming: Naming, schemas: Json.Node[?, ?]*): List[Typescript.Statement.Declaration] =
    val read = definitions(Side.Read, JsonTypescriptEffect.Context, schemas)
    val write = definitions(Side.Write, JsonTypescriptEffect.Context.copy(names = read.names), schemas)
    val names = write.names
    val split = naming match
      case Naming.Suffixed  => names.names.toSet
      case Naming.Collapsed => collapse(names, schemas, Set.empty)

    val context = allocated(names, split)
    val readers = definitions(Side.Read, context, schemas)
    val writers = definitions(Side.Write, context, schemas)

    readers.declarations ++ writers
      .copy(
        definitions = writers.definitions.filterNot((name, _) => readers.definitions.contains(name))
      )
      .declarations

  @tailrec
  private def collapse(names: DefinitionNames, schemas: Seq[Json.Node[?, ?]], split: Set[String]): Set[String] =
    val context = allocated(names, split)
    val read = definitions(Side.Read, context, schemas)
    val write = definitions(Side.Write, context, schemas)
    val differ = names.names.filter: base =>
      val reader = context.bindings.get((base, Side.Read)).flatMap(read.definitions.get)
      val writer = context.bindings.get((base, Side.Write)).flatMap(write.definitions.get)

      !split.contains(base) && reader.isDefined && writer.isDefined && reader != writer

    if differ.isEmpty then split else collapse(names, schemas, split ++ differ)

  private def allocated(names: DefinitionNames, split: Set[String]): JsonTypescriptContext =
    names.names.foldLeft(JsonTypescriptEffect.Context.copy(names = names)): (context, base) =>
      if split.contains(base) then
        val read = context.available(base + suffix(Side.Read))
        val updated = context.bind(base, Side.Read, read)
        updated.bind(base, Side.Write, updated.available(base + suffix(Side.Write)))
      else
        val name = context.available(base)
        context.bind(base, Side.Read, name).bind(base, Side.Write, name)

  private def definitions(
      side: Side,
      context: JsonTypescriptContext,
      schemas: Seq[Json.Node[?, ?]]
  ): JsonTypescriptContext =
    val renderer = stateful(side, identity)

    schemas.toList.traverse_(schema => renderer.render(schema)).runS(context).value

  private def suffix(side: Side): String = side match
    case Side.Read  => "Read"
    case Side.Write => "Write"

  private def stateful(
      side: Side,
      rename: String => String
  ): Renderer[Json.Node, State[JsonTypescriptContext, Typescript.Expression]] = JsonStateTypescriptRenderer(
    side,
    JsonTypescriptEffect.Namespaces,
    JsonTypescriptEffect.Target,
    rename,
    JsonTypescriptExpressionEffectRenderer(side, _)
  )

  private def rendered(side: Side): Renderer[Json.Node, List[Typescript]] = stateful(side, identity)
    .map(_.run(JsonTypescriptEffect.Context).value)
    .map((context, expression) => context.declarations :+ expression)
