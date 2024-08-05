// package io.taig.otter.sample.data

// import cats.Order
// import cats.syntax.all.*
// import io.taig.otter.http.Dsl.*

// import scala.collection.immutable.SortedSet
// import io.circe.Json

// final case class Book(isbn: Isbn, title: Book.Title, genres: SortedSet[Book.Genre], metadata: Json)

// object Book:
//   opaque type Title = String
//   // object Title:
//   //   def unsafeFromString(value: String): Book.Title = value
//   //   val validation: CodecValidation.Primitive[String, Book.Title] = (minLength(1) *> maxLength(200)).tap

//   enum Genre:
//     case Biography
//     case Children
//     case Fantasy
//     case Poetry
//     case Romance
//     case Thriller

//   object Genre:
//     given Order[Book.Genre] = Order.by(_.ordinal)
