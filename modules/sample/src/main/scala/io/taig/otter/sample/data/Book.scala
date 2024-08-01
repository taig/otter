// package io.taig.otter.sample.data

// import cats.Order
// import cats.syntax.all.*
// import io.circe.Json
// import io.taig.otter.validation.Validation
// import io.taig.otter.validation.validations.{maxLength, minLength}

// import scala.collection.immutable.SortedSet

// final case class Book(isbn: Isbn, title: Book.Title, genres: SortedSet[Book.Genre], metadata: Json)

// object Book:
//   opaque type Title = String
//   object Title:
//     def unsafeFromString(value: String): Book.Title = value
//     val validation: Validation[String, Book.Title] = (minLength(1) *> maxLength(200)).tap

//   enum Genre:
//     case Biography
//     case Children
//     case Fantasy
//     case Poetry
//     case Romance
//     case Thriller

//   object Genre:
//     given Order[Book.Genre] = Order.by:
//       case Biography => 0
//       case Children  => 1
//       case Fantasy   => 2
//       case Poetry    => 3
//       case Romance   => 4
//       case Thriller  => 5
