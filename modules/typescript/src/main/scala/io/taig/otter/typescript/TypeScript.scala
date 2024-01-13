package io.taig.otter.typescript

import io.taig.otter.*
import java.lang.StringBuilder

trait TypeScript:
  def apply(codec: Codec[?]): String

object TypeScript:
  val Default: TypeScript = new TypeScript:
    override def apply(codec: Codec[?]): String =
      val builder = new StringBuilder
      apply(codec, level = 0, builder)
      builder.toString

    def apply(codec: Codec[?], level: Int, builder: StringBuilder): Unit =
      codec match
        case codec: Primitive[?] => apply(codec, builder)
        case codec: Record[?]    => apply(codec, level = level + 1, builder)
        case _                   => ???
      ()

    def apply(codec: Primitive[?], builder: StringBuilder): Unit =
      builder.append(apply(codec.tpe))
      ()

    def apply(tpe: Type[?]): String = tpe match
      case Type.BigDecimal => "number"
      case Type.BigInt     => "number"
      case Type.Boolean    => "boolean"
      case Type.Double     => "number"
      case Type.Float      => "number"
      case Type.Int        => "number"
      case Type.Long       => "number"
      case Type.String     => "string"

    def apply(codec: Record[?], level: Int, builder: StringBuilder): Unit =
      builder.append("{\n")

      val fields = codec.toChain.zipWithIndex.toList
      fields.foreach:
        case (field, index) =>
          builder.append("  " * level)
          builder.append('"')
          builder.append(field.name)
          builder.append('"')
          builder.append(": ")
          apply(field.codec, level, builder)
          builder.append("}")

          if index < fields.length then builder.append(",\n")
          else builder.append("\n")
