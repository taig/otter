package io.taig.otter

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Keys.*
import io.taig.otter.ZodCodecPrinter.References

import scala.collection.immutable.ListMap
import scala.collection.immutable.SortedMap

final class ZodCodecPrinter(imports: List[String]):
  def print(codecs: List[Codec[?, ?]]): String =
    val references = codecs
      .filter(_.metadata.contains(name))
      .traverse(referenceOrRender)
      .runS(References.Empty)
      .value

    val body = references.map:
      case ((Some(namespace), name), reference) =>
        s"""namespace $namespace {
           |${indent(render(name, reference))}
           |}""".stripMargin
      case ((None, name), reference) => render(name, reference)

    s"""${("""import { z } from "zod"""" :: imports).mkString("\n")}
       |
       |${body.mkString("\n\n")}""".stripMargin

  def print(codec: Codec[?, ?]): String = print(codec :: Nil)

  def render(codec: Codec[?, ?]): State[References, String] = codec.typescript.value match
    case Some(typescript) => State.pure(typescript)
    case None =>
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

  def render(name: String, value: String): String =
    val symbol = toSymbol(name)
    s"""export type $symbol = z.infer<typeof $symbol>
       |export const $symbol = $value""".stripMargin

  def toSymbol(name: String): String = name.replace(".", "")

  def referenceOrRender(codec: Codec[?, ?]): State[References, String] = codec.name.value match
    case Some(name) =>
      State: references =>
        val namespace = codec.namespace.value
        val symbol = toSymbol(name)
        if references.contains((namespace, name))
        then (references, symbol)
        else
          val (nestedReferences, value) = render(codec).run(references).value
          (nestedReferences ++ references.updated((namespace, name), value), symbol)
    case None => render(codec)

  def render(data: Data.Primitive): String = Printers(data, quoted = true)

  def render(codec: Constant[?, ?]): String = s"z.literal(${render(codec.data)})"

  def render(codec: Primitive[?, ?]): String = codec.tpe match
    case Type.Boolean   => "z.boolean()"
    case _: Type.Number => "z.number()"
    case _: Type.String => "z.string()"

  def render(codec: Enumeration[?, ?]): String =
    val values = codec.data.map(render).mkString_(", ")
    s"z.enum([$values])"

  def render(codec: Record[?, ?]): State[References, String] = codec.fields
    .traverse: field =>
      referenceOrRender(field.codec.value).map: reference =>
        s""""${field.name}": $reference"""
    .map: fields =>
      s"""z.object({
         |${indent(fields.mkString_(",\n"))}
         |})""".stripMargin

  def render(codec: Collection[?, ?]): State[References, String] =
    val minItems = codec.constraints.collectFirst { case Constraint.Collection.MinItems(value) => value }
    val maxItems = codec.constraints.collectFirst { case Constraint.Collection.MaxItems(value) => value }
    val nonEmpty = minItems.fold(false)(_ >= 1)

    val size = (minItems, maxItems) match
      case (Some(min), Some(max)) if min === max => s".length($min)"
      case _ =>
        minItems.filter(_ > 1).map(min => s".min($min)").orEmpty + maxItems.map(max => s".max($max)").orEmpty

    referenceOrRender(codec.codec.value)
      .map(reference => s"z.array($reference)")
      .map(_ + (if nonEmpty then ".nonempty()" else "") + size)

  def render(codec: Dictionary[?, ?]): State[References, String] = for
    key <- referenceOrRender(codec.key.value)
    value <- referenceOrRender(codec.codec.value)
  yield s"z.map($key, $value)"

  def render(codec: Nullable[?, ?]): State[References, String] =
    referenceOrRender(codec.codec.value).map: reference =>
      s"z.optional($reference)"

  def render(codec: Union[?, ?]): State[References, String] =
    if codec.branches.length === 1L
    then referenceOrRender(codec.branches.head.codec.value)
    else
      codec.branches
        .traverse(branch => referenceOrRender(branch.codec.value))
        .map(branches => s"z.union([${branches.mkString_(", ")}])")

  def render(codec: Dynamic[?, ?]): String = "z.any()"

  def indent(value: String): String = value.linesIterator.map("  " + _).mkString("\n")

object ZodCodecPrinter:
  def apply(imports: List[String] = Nil): ZodCodecPrinter = new ZodCodecPrinter(imports)

  private type References = ListMap[(Option[String], String), String]

  private object References:
    val Empty: References = ListMap.empty
