// package io.taig.otter.json.circe

// import io.taig.otter as Base
// import cats.data.Chain
// import cats.syntax.all.*
// import io.circe.Json
// import io.taig.otter.Collection
// import io.taig.otter.Fix
// import scala.annotation.targetName
// import io.taig.otter.Write
// import io.taig.otter.Schema

// object JsonCollectionEncoder:
//   def apply[Of, A](schema: Collection[Write, Of, A], a: A): Option[Chain[Json]] = schema match
//     case Collection.Root(schema, unwrap)           => ???
//     case Collection.Optional(self)                 => a.flatMap(apply(self, _))
//     case Collection.Invariant(self, validation, f) => ???
//     case Collection.Contravariant(self, f)         => ???
//     // case Collection.Functor(self, validation)      => ???

//   val x: Collection.Functor[Any, Any, Any, Any, Any] = ???
//   val y: Collection.Contravariant[Any, Any, Any] = ???

// //   def apply[A](schema: Plain.Collection.Writer[A], a: A): Option[Chain[Json]] = apply(schema.unfix, a)

// //   @targetName("applyBase")
// //   def apply[Of, A](schema: Base.Collection.Writer[Of, A], a: A): Option[Chain[Json]] = schema match
// //     case Base.Collection.Root(schema, writer)        => a.map(JsonEncoder(writer(schema), _)).some
// //     case Base.Collection.Writer.Root(schema, writer) => a.map(JsonEncoder(writer(schema), _)).some
// //     case Base.Collection.Validate(self, _, f)        => apply(self, f(a))
// //     case Base.Collection.Writer.Modify(self, f)      => apply(self, f(a))
// //     case Base.Collection.Optional(self)              => a.flatMap(apply(self, _))
// //     case Base.Collection.Writer.Optional(self)       => a.flatMap(apply(self, _))
