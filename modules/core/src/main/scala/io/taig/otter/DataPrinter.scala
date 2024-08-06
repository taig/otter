package io.taig.otter

import io.taig.otter.Data.Null

object DataPrinter:
  def apply(data: Data, quoted: Boolean): String = data match
    case Data.Object(values) =>
      s"{${values.map { case (key, value) => s"\"$key\":${DataPrinter(value, quoted)}" }.mkString(",")}}"
    case Data.Array(values)  => s"[${values.map(DataPrinter(_, quoted).mkString(","))}]"
    case Data.String(value)  => if quoted then "\"$value\"" else value
    case Data.Boolean(value) => String.valueOf(value)
    case Data.Number(value)  => String.valueOf(value)
    case Data.Null           => "null"
