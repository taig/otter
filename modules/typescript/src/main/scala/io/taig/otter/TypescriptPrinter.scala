package io.taig.otter

import cats.syntax.all.*

object TypescriptPrinter:
  def apply(codec: Codec[?, ?]): String = codec match
    case codec: Collection[?, ?]  => apply(codec)
    case codec: Dictionary[?, ?]  => apply(codec)
    case codec: Enumeration[?, ?] => apply(codec)
    case codec: Primitive[?, ?]   => apply(codec)
    case codec: Record[?, ?]      => apply(codec)
    case codec                    => s"<Unsupported codec: ${codec.getClass.getSimpleName}>"

  def apply(data: Data.Primitive): String = Printers(data, quoted = true)

  def apply(codec: Primitive[?, ?]): String = codec.tpe match
    case Type.Boolean => "z.boolean()"
    case Type.Number  => "z.number()"
    case Type.String  => "z.string()"

  def apply(codec: Enumeration[?, ?]): String =
    val values = codec.data.map(apply).mkString_(", ")
    s"z.enum([$values])"

  def apply(codec: Record[?, ?]): String =
    val fields = codec.fields.map(field => s"""  "${field.name}": ${apply(field.codec.value)}""")
    s"""z.object({
       |${fields.mkString_(",\n")}
       |})""".stripMargin

  def apply(codec: Collection[?, ?]): String =
    val minItems = codec.constraints.collectFirst { case Constraint.Collection.MinItems(value) => value }
    val maxItems = codec.constraints.collectFirst { case Constraint.Collection.MaxItems(value) => value }
    val uniqueItems = codec.constraints.collectFirst { case Constraint.Collection.UniqueItems => true }.getOrElse(false)
    val nonEmpty = minItems.fold(false)(_ >= 1)

    val tpe = if uniqueItems then s"z.set(${apply(codec.codec.value)})" else s"z.array(${apply(codec.codec.value)})"

    val size = (minItems, maxItems) match
      case (Some(min), Some(max)) if min === max => s".length($min)"
      case _ =>
        minItems.filter(_ > 1).map(min => s".min($min)").orEmpty + maxItems.map(max => s".max($max)").orEmpty

    tpe + (if nonEmpty then ".nonempty()" else "") + size

  def apply(codec: Dictionary[?, ?]): String = s"z.map(${apply(codec.key.value)}, ${apply(codec.codec.value)})"
