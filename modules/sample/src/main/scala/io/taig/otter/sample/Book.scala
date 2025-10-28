package io.taig.otter.sample

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import java.time.Instant

final case class Book(
    created: Instant,
    author: String,
    genre: Genre,
    isbn: String,
    reference: Book.Reference,
    title: String
)

object Book:
  type Reference = String :| FixedLength[Reference.Length.type]

  object Reference:
    inline val Length = 16
