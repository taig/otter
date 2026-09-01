package io.taig.otter.codec

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.JsonTypescriptEffect
import io.taig.otter.Side
import io.taig.otter.Typescript

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

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
  * not on a wrapper around it: the name is the only thing a cycle can be broken with, and an anonymous cycle does not
  * terminate.
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

  def module(naming: Naming, schemas: Json.Node[?, ?]*): List[Typescript.Statement.Declaration] =
    val names = definitions(Side.Read, Set.empty, schemas).keySet ++ definitions(Side.Write, Set.empty, schemas).keySet

    val split = naming match
      case Naming.Suffixed  => names
      case Naming.Collapsed => collapse(names, schemas, Set.empty)

    val read = definitions(Side.Read, split, schemas)
    val write = definitions(Side.Write, split, schemas)

    /* The read side's order already puts every declaration after what it refers to, and a write side declaration can
     * only refer to something shared, which the read side has therefore already emitted. */
    declarations(read) ++ declarations(write.filterNot((name, _) => read.contains(name)))

  /** Which names have to be split, as the least set that is closed under the splitting it causes.
    *
    * Comparing the two sides once is not enough: a record whose own shape is the same on both sides still differs when
    * a definition it refers to had to be split, because it then refers to two different names. Rather than analyse
    * which name reaches which, the whole thing is rendered again under the names decided so far, until a pass finds
    * nothing new. Each pass can only add, and there are finitely many names, so it stops.
    */
  @tailrec
  private def collapse(names: Set[String], schemas: Seq[Json.Node[?, ?]], split: Set[String]): Set[String] =
    val read = definitions(Side.Read, split, schemas)
    val write = definitions(Side.Write, split, schemas)

    val differ = names.filter: name =>
      !split.contains(name) && read.contains(name) && write.contains(name) && read(name) != write(name)

    if differ.isEmpty then split else collapse(names, schemas, split ++ differ)

  private def definitions(
      side: Side,
      split: Set[String],
      schemas: Seq[Json.Node[?, ?]]
  ): ListMap[String, JsonTypescriptDefinition] =
    val renderer = stateful(side, name => if split.contains(name) then name + suffix(side) else name)

    schemas.toList
      .traverse_(schema => renderer.render(schema))
      .runS(JsonTypescriptContext.Empty)
      .value
      .definitions

  private def declarations(
      definitions: ListMap[String, JsonTypescriptDefinition]
  ): List[Typescript.Statement.Declaration] =
    definitions.toList.flatMap((name, definition) => definition.declarations(name))

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
    .map(_.run(JsonTypescriptContext.Empty).value)
    .map((context, expression) => context.declarations :+ expression)
