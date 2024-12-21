package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.Keys.*
import cats.data.State
import scala.collection.immutable.ListMap

object ZodCodecPrinter:
  def print(codecs: List[Codec[?, ?]]): String =
    val references = codecs.filter(_.apply(name).isDefined).traverse(referenceOrRender).runS(ListMap.empty).value
    references.map(render(_, _)).mkString("\n\n")

  def print(codec: Codec[?, ?]): String = print(codec :: Nil)

  def render(codec: Codec[?, ?]): State[ListMap[String, String], String] = codec match
    case codec: Collection[?, ?]  => render(codec)
    case codec: Dictionary[?, ?]  => render(codec)
    case codec: Enumeration[?, ?] => State.pure(render(codec))
    case codec: Primitive[?, ?]   => State.pure(render(codec))
    case codec: Constant[?, ?]    => State.pure(render(codec))
    case codec: Record[?, ?]      => render(codec)
    case codec: Nullable[?, ?]    => render(codec)
    case codec                    => State.pure(s"<Unsupported codec: ${codec.getClass.getName}>")

  def render(name: String, value: String): String =
    val symbol = toSymbol(name)
    s"""type $symbol = z.infer<typeof $symbol>
       |const $symbol = $value""".stripMargin

  def toSymbol(name: String): String = name.replace(".", "")

  def referenceOrRender(codec: Codec[?, ?]): State[ListMap[String, String], String] = codec(name) match
    case Some(name) =>
      State: references =>
        val symbol = toSymbol(name)
        if references.contains(name)
        then (references, symbol)
        else
          val (nestedReferences, value) = render(codec).run(references).value
          (nestedReferences ++ references.updated(name, value), symbol)
    case None => render(codec)

  def render(data: Data.Primitive): String = Printers(data, quoted = true)

  def render(codec: Constant[?, ?]): String = s"z.literal(${render(codec.data)})"

  def render(codec: Primitive[?, ?]): String = codec.tpe match
    case Type.Boolean => "z.boolean()"
    case Type.Number  => "z.number()"
    case Type.String  => "z.string()"

  def render(codec: Enumeration[?, ?]): String =
    val values = codec.data.map(render).mkString_(", ")
    s"z.enum([$values])"

  def render(codec: Record[?, ?]): State[ListMap[String, String], String] = codec.fields
    .traverse: field =>
      referenceOrRender(field.codec.value).map: reference =>
        s""""${field.name}": $reference"""
    .map: fields =>
      s"""z.object({
         |${indent(fields.mkString_(",\n"))}
         |})""".stripMargin

  def render(codec: Collection[?, ?]): State[ListMap[String, String], String] =
    val minItems = codec.constraints.collectFirst { case Constraint.Collection.MinItems(value) => value }
    val maxItems = codec.constraints.collectFirst { case Constraint.Collection.MaxItems(value) => value }
    val uniqueItems = codec.constraints.collectFirst { case Constraint.Collection.UniqueItems => true }.getOrElse(false)
    val nonEmpty = minItems.fold(false)(_ >= 1)

    val size = (minItems, maxItems) match
      case (Some(min), Some(max)) if min === max => s".length($min)"
      case _ =>
        minItems.filter(_ > 1).map(min => s".min($min)").orEmpty + maxItems.map(max => s".max($max)").orEmpty

    referenceOrRender(codec.codec.value)
      .map: reference =>
        if uniqueItems then s"z.set($reference)" else s"z.array($reference)"
      .map(_ + (if nonEmpty then ".nonempty()" else "") + size)

  def render(codec: Dictionary[?, ?]): State[ListMap[String, String], String] =
    for
      key <- referenceOrRender(codec.key.value)
      value <- referenceOrRender(codec.codec.value)
    yield s"z.map($key, $value)"

  def render(codec: Nullable[?, ?]): State[ListMap[String, String], String] =
    referenceOrRender(codec.codec.value).map: reference =>
      s"z.optional($reference)"

  def indent(value: String): String = value.linesIterator.map("  " + _).mkString("\n")
