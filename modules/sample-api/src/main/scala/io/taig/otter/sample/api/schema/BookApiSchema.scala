// package io.taig.otter.sample.api.schema

// import cats.Order
// import cats.implicits.*
// import io.taig.otter.sample.api.Dsl.*

// import scala.collection.immutable.SortedSet

// final case class BookApiSchema(
//     isbn: IsbnApiSchema,
//     title: String,
//     genres: SortedSet[BookApiSchema.Genre],
//     metadata: Data.Object[?]
// ):
//   def toBookApiSchemaSummary: BookApiSchema.Summary = BookApiSchema.Summary(isbn, title)

// object BookApiSchema:
//   enum Genre:
//     case Biography
//     case Children
//     case Fantasy
//     case Poetry
//     case Romance
//     case Thriller

//   object Genre:
//     val codec: Enumeration[BookApiSchema.Genre] = enumeration(string):
//       case Biography => "biography"
//       case Children  => "children"
//       case Fantasy   => "fantasy"
//       case Poetry    => "poetry"
//       case Romance   => "romance"
//       case Thriller  => "thriller"

//     given Order[BookApiSchema.Genre] = Order.by(_.ordinal)

//   final case class Summary(isbn: IsbnApiSchema, title: String)

//   object Summary:
//     val codec: Record.Of[Data.Primitive, BookApiSchema.Summary] =
//       (field("isbn", IsbnApiSchema.codec) :* field("title", string)).to

//   final case class Create(
//       isbn: IsbnApiSchema,
//       title: String,
//       genres: SortedSet[BookApiSchema.Genre],
//       metadata: Data.Object[?]
//   )

//   object Create:
//     val codec: Record[BookApiSchema.Create] = (
//       field("isbn", IsbnApiSchema.codec) :*
//         field("title", string(minLength = 1.some, maxLength = 500.some)) :*
//         field("genres", collection.sortedSet(Genre.codec).nullable(SortedSet.empty[Genre])) :*
//         field("metadata", dynamic.obj.nullable(Data.Object.Empty))
//     ).to

//   val codec: Record[BookApiSchema] = (
//     field("isbn", IsbnApiSchema.codec) :*
//       field("title", string) :*
//       field("genres", collection.sortedSet(Genre.codec)) :*
//       field("metadata", dynamic.obj)
//   ).to
