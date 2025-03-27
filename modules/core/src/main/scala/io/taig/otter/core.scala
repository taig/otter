package io.taig.otter

import cats.Eq

import java.util.regex.Pattern

private[otter] given Eq[Data.Number] = Eq.fromUniversalEquals
private[otter] given Eq[Data.Primitive] = Eq.fromUniversalEquals
private[otter] given Eq[Data.Value] = Eq.fromUniversalEquals
private[otter] given Eq[Data.Any] = Eq.fromUniversalEquals

private[otter] given Eq[Pattern] = Eq.by(_.pattern)
