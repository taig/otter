package io.taig.otter

private[otter] def indent(value: String): String = value.linesIterator.map("  " + _).mkString("\n")
