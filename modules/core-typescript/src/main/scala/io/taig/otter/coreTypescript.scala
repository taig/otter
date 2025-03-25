package io.taig.otter

private[otter] def indent(value: String): String = value.linesIterator.map("  " + _).mkString("\n")

private[otter] def symbol(name: String): String = name.replace(".", "")
