package io.taig.otter

private[otter] def toSymbol(value: String): String = value.replace(".", "").replace(" ", "")
