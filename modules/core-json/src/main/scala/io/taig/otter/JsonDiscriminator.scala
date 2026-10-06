package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.component.JsonComponent

/** Tagged branches keep their wire record and their dispatch key together. Payload references remain lazy, so a tagged
  * branch may refer back to the union that contains it.
  */
object JsonDiscriminator:
  final case class Nested(tag: String = "type", value: String = "value"):
    require(tag != value, "The discriminator and value keys must differ")

  final case class Merged(tag: String = "type")

  private val Key: Metadata.Key[String] = Metadata.Key("discriminator")

  def tag(branch: Json.Branch.Node[?, ?]): Option[String] = branch.self.metadata.get(Json.Namespace, Key)

  private[otter] def nested[W, R](
      name: String,
      schema: => Json.Node[W, R],
      discriminator: JsonDiscriminator.Nested
  ): Json.Branch.Schema[Json.Record.Node, W, R] =
    tagged(name, discriminator.tag):
      val payload = schema
      val fields = payload match
        case Json.Tuple.Schema(node) => empty(node.self).getOrElse(value(discriminator.value, payload))
        case _                       => value(discriminator.value, payload)
      record(name, discriminator.tag, Annotation(fields))

  private[otter] def merged[W, R](
      name: String,
      schema: => Json.Record.Node[W, R],
      discriminator: JsonDiscriminator.Merged
  ): Json.Branch.Schema[Json.Record.Node, W, R] =
    val payload = schema
    require(
      !payload.self.self.fields.exists(_.value.self.self.name == discriminator.tag),
      s"Discriminator key '${discriminator.tag}' collides with a field in branch '$name'"
    )
    tagged(name, discriminator.tag)(record(name, discriminator.tag, payload.self))

  private def tagged[W, R](
      name: String,
      tag: String
  )(schema: => Json.Record.Node[W, R]): Json.Branch.Schema[Json.Record.Node, W, R] =
    Json.Branch.Schema(
      Annotation(Metadata.one(Json.Namespace, Key, tag), Branch.Root(name, Reference.later(schema)))
    )

  private def record[W, R](
      name: String,
      tag: String,
      payload: Annotation[Record[Json.Field.Node, W, R]]
  ): Json.Record.Node[W, R] =
    val field = JsonComponent.field(tag, JsonComponent.constant(JsonComponent.string, name))
    val product = Record.Product(Record.Root(Reference.now(field)), payload.self)
    Json.Record.Schema(payload.map(_ => Record.Modify(product, (_: (Unit, R))._2, (w: W) => ((), w))))

  private def value[W, R](name: String, schema: Json.Node[W, R]): Record[Json.Field.Node, W, R] =
    Record.Root(Reference.now(Json.Field.Schema(Field.Root(name, Reference.now(schema)))))

  /** The empty tuple is the JSON vocabulary's unit. Preserve conversions to singleton cases while omitting its field.
    */
  private def empty[W, R](schema: Tuple[Json.Node, W, R]): Option[Record[Json.Field.Node, W, R]] = schema match
    case Tuple.Empty                => Some(Record.Empty)
    case Tuple.Modify(self, f, g)   => empty(self).map(Record.Modify(_, f, g))
    case Tuple.Product(left, right) => (empty(left), empty(right)).mapN(Record.Product(_, _))
    case _                          => None

  /** Validate only branch declarations here; forcing a payload would break recursive definitions. */
  private[otter] def validate(schema: Union[Json.Branch.Node, ?, ?]): Option[String] =
    val branches = schema.branches.toNonEmptyList.map(_.value)
    val tags = branches.map(JsonDiscriminator.tag)
    require(tags.toList.distinct.size == 1, "A union must use one discriminator key, or be entirely untagged")
    val tag = tags.head
    if tag.isDefined then
      val names = branches.map(_.self.self.name).toList
      require(names.distinct.size == names.size, "Discriminator branch names must be unique")
    tag

  /** Lift the union's conversions into each reader once. Looking up a tag then reads exactly one payload. */
  private[otter] def readers[R](schema: Union[Json.Branch.Node, Nothing, R]): Map[String, Json.Reader[R]] =
    schema match
      case Union.Root(reference)        => Map(reference.value.self.self.name -> reader(reference.value.self.self))
      case Union.Modify(self, f, _)     => readers(self).view.mapValues(_.map(f)).toMap
      case Union.Coproduct(left, right) =>
        alternatives(readers(left), readers(right))

  private def alternatives[A, B](
      left: Map[String, Json.Reader[A]],
      right: Map[String, Json.Reader[B]]
  ): Map[String, Json.Reader[Either[A, B]]] =
    left.view.mapValues(_.map(Left(_): Either[A, B])).toMap ++
      right.view.mapValues(_.map(Right(_): Either[A, B])).toMap

  private def reader[R](schema: Branch[Json.Node, Nothing, R]): Json.Reader[R] = schema match
    case Branch.Root(_, reference) => reference.value
    case Branch.Modify(self, f, _) => reader(self).map(f)
