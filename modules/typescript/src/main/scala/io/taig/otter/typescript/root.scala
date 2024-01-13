package io.taig.otter.typescript

import cats.syntax.all.*
import cats.data.Chain
import io.taig.otter.*
import scala.collection.immutable.SortedMap
import io.taig.otter.Data.Null

def toExportedTypes(codecs: Chain[Codec[?]]): String =
  codecs.map(toExportedTypes).mkString_("\n\n")

def toExportedTypes(codec: Codec[?]): String = toNamedTypes(codec)
  .map:
    case (name, tpe) => s"export type ${toTypeName(name)} = $tpe"
  .mkString("\n\n")

def toTypeName(name: String): String = name.replaceAll("\\.", "")

def toNamedTypes(codecs: Chain[Codec[?]]): SortedMap[String, String] =
  codecs.map(toNamedTypes).foldLeft(SortedMap.empty[String, String])(_ ++ _)

def toNamedTypes(codec: Codec[?]): SortedMap[String, String] =
  toNamedCodecs(codec).map { case (name, codec) => name -> toType(codec) }

def toNamedCodecs(codecs: Chain[Codec[?]]): SortedMap[String, Codec[?]] =
  codecs.map(toNamedCodecs).foldLeft(SortedMap.empty[String, Codec[?]])(_ ++ _)

def toNamedCodecs(codec: Codec[?]): SortedMap[String, Codec[?]] =
  codec.name.fold(SortedMap.empty[String, Codec[?]])(name => SortedMap(name -> codec)) ++
    toChildren(codec).foldLeft(SortedMap.empty[String, Codec[?]])((registry, codec) => registry ++ toNamedCodecs(codec))

def toChildren(codec: Codec[?]): Chain[Codec[?]] = codec match
  case _: Primitive[?]       => Chain.empty
  case codec: Collection[?]  => Chain.one(codec.codec)
  case codec: Coproduct[?]   => ???
  case codec: Dictionary[?]  => ???
  case codec: Dynamic[?]     => Chain.empty
  case codec: Enumeration[?] => Chain.one(codec.codec)
  case codec: Product[?]     => codec.toChain
  case codec: Record[?]      => codec.toChain.map(_.codec)
  case codec: Union[?]       => ???

def toNameOrType(codec: Codec[?]): String = codec.name.map(toTypeName).getOrElse(toType(codec))

def toType(codec: Codec[?]): String = codec match
  case codec: Collection[?]  => toType(codec)
  case codec: Coproduct[?]   => ???
  case codec: Dictionary[?]  => ???
  case codec: Dynamic[?]     => "any"
  case codec: Enumeration[?] => toType(codec)
  case codec: Primitive[?]   => toType(codec.tpe)
  case codec: Product[?]     => ???
  case codec: Record[?]      => toType(codec)
  case codec: Union[?]       => ???

def toType(tpe: Type[?]): String = tpe match
  case Type.BigDecimal => "number"
  case Type.BigInt     => "number"
  case Type.Boolean    => "boolean"
  case Type.Double     => "number"
  case Type.Float      => "number"
  case Type.Int        => "number"
  case Type.Long       => "number"
  case Type.String     => "string"

def toType(codec: Collection[?]): String = s"${toNameOrType(codec.codec)}[]"

def toType(codec: Enumeration[?]): String = codec.values.map(toType).mkString_(" | ")

def toType(data: Data): String = data match
  case Data.Object(values) => ???
  case Data.Array(values)  => ???
  case Data.String(value)  => s"\"$value\""
  case Data.Boolean(value) => String.valueOf(value)
  case Data.Number(value)  => value.toString()
  case Null                => "null"

def toType(codec: Record[?]): String = codec.toChain
  .map(field => s"  \"${field.name}\": ${toNameOrType(field.codec)}")
  .mkString_("{\n", ",\n", "\n}")
