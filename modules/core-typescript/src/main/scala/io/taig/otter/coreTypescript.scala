package io.taig.otter

import cats.syntax.all.*

private[otter] def toSymbol(value: String): String = value.replace(".", "").replace(" ", "")
