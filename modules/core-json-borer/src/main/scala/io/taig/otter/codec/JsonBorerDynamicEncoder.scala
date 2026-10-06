package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Json

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object JsonBorerDynamicEncoder:
  def encode[W](schema: Json.Dynamic.Node[W, Any], value: W): BorerWrite =
    schema.write(value)(data, data, data)

  private def data(value: Data): BorerWrite = value match
    case Data.Null          => BorerWrite(_.writeNull())
    case value: Boolean     => BorerWrite(_.writeBoolean(value))
    case value: String      => BorerWrite(_.writeString(value))
    case value: Int         => BorerWrite(_.writeInt(value))
    case value: Long        => BorerWrite(_.writeLong(value))
    case value: JBigInteger => BorerWrite(_.writeNumberString(value.toString))
    case value: JBigDecimal => BorerWrite(_.writeNumberString(value.toString))
    case value: Float       =>
      if value.isNaN || value.isInfinite then BorerWrite(_.writeString(value.toString))
      else BorerWrite(_.writeNumberString(value.toString))
    case value: Double =>
      if value.isNaN || value.isInfinite then BorerWrite(_.writeString(value.toString))
      else BorerWrite(_.writeNumberString(value.toString))
    case value: Data.Array[Data] =>
      val elements = value.values.foldLeft(BorerWrite.Empty)((write, item) => write |+| data(item))
      BorerWrite(writer => elements.write(writer.writeArrayStart()).writeBreak())
    case value: Data.Object[Data] =>
      val members = value.values.foldLeft(BorerWrite.Empty) { case (write, (key, item)) =>
        write |+| BorerWrite(writer => data(item).write(writer.writeString(key)))
      }
      BorerWrite(writer => members.write(writer.writeMapStart()).writeBreak())
