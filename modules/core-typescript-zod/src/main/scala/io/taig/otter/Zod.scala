package io.taig.otter

import cats.derived.*
import cats.Functor

final case class Zod[A](typescript: Typescript, expression: A) derives Functor
