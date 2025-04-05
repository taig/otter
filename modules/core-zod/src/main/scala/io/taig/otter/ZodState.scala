package io.taig.otter

import cats.data.State
import scala.collection.immutable.ListMap

type ZodState[A] = State[ListMap[Const, String], A]
