package io.taig.otter

import cats.data.State
import cats.syntax.all.*

import scala.collection.immutable.ListMap

object ZodCodecPrinter extends CodecPrinter[State[ListMap[Reference, String], *]]:
  override def print(codec: Codec[?, ?]): State[ListMap[Reference, String], Expression] =
    codec.name.value match
      case Some(name) =>
        State: references =>
          val reference: Reference = Reference(namespace = codec.namespace.value, name = symbol(name))

          references.get(reference) match
            case Some(value) => (references, Expression.Referenced(reference, value))
            case None        =>
              val (nestedReferences, value) = render(codec).run(references).value
              (nestedReferences ++ references.updated(reference, value), Expression.Referenced(reference, value))
      case None => render(codec).map(Expression.Inline.apply)

  def render(codec: Codec[?, ?]): State[ListMap[Reference, String], String] = codec.typescript.value match
    case Some(typescript) => State.pure(typescript)
    case None             =>
      codec match
        case codec: Collection[?, ?]  => render(codec)
        case codec: Dictionary[?, ?]  => render(codec)
        case codec: Enumeration[?, ?] => State.pure(render(codec))
        case codec: Primitive[?, ?]   => State.pure(render(codec))
        case codec: Constant[?, ?]    => State.pure(render(codec))
        case codec: Record[?, ?]      => render(codec)
        case codec: Union[?, ?]       => render(codec)
        case codec: Nullable[?, ?]    => render(codec)
        case codec: Dynamic[?, ?]     => State.pure(render(codec))
        case codec                    => State.pure(s"<Unsupported codec: ${codec.getClass.getName}>")

  def render(data: Data.Primitive): String = Printers(data, quoted = true)

  def render(codec: Constant[?, ?]): String = s"z.literal(${render(codec.data)})"

  def render(codec: Primitive[?, ?]): String = codec.tpe match
    case Type.Boolean   => "z.boolean()"
    case _: Type.Number => "z.number()"
    case _: Type.String => "z.string()"

  def render(codec: Enumeration[?, ?]): String =
    val values = codec.data.map(render).mkString_(", ")
    s"z.enum([$values])"

  def render(codec: Record[?, ?]): State[ListMap[Reference, String], String] = codec.fields
    .traverse: field =>
      print(field.codec.value).map: reference =>
        show""""${field.name}": $reference"""
    .map: fields =>
      s"""z.object({
         |${indent(fields.mkString_(",\n"))}
         |})""".stripMargin

  def render(codec: Collection[?, ?]): State[ListMap[Reference, String], String] =
    val minItems = codec.constraints.collectFirst { case Constraint.Collection.MinItems(value) => value }
    val maxItems = codec.constraints.collectFirst { case Constraint.Collection.MaxItems(value) => value }
    val nonEmpty = minItems.fold(false)(_ >= 1)

    val size = (minItems, maxItems) match
      case (Some(min), Some(max)) if min === max => s".length($min)"
      case _                                     =>
        minItems.filter(_ > 1).map(min => s".min($min)").orEmpty + maxItems.map(max => s".max($max)").orEmpty

    print(codec.codec.value)
      .map(reference => show"z.array($reference)")
      .map(_ + (if nonEmpty then ".nonempty()" else "") + size)

  def render(codec: Dictionary[?, ?]): State[ListMap[Reference, String], String] = for
    key <- print(codec.key.value)
    value <- print(codec.codec.value)
  yield show"z.map($key, $value)"

  def render(codec: Nullable[?, ?]): State[ListMap[Reference, String], String] =
    print(codec.codec.value).map: reference =>
      show"z.optional($reference)"

  def render(codec: Union[?, ?]): State[ListMap[Reference, String], Expression] =
    if codec.branches.length === 1L
    then print(codec.branches.head.codec.value)
    else
      codec.branches
        .traverse(branch => print(branch.codec.value))
        .map(branches => s"z.union([${branches.mkString_(", ")}])")
        .map(Expression.Inline.apply)

  def render(codec: Dynamic[?, ?]): String = "z.any()"
