// package io.taig.otter.codec

// import cats.syntax.all.*
// import io.taig.otter.Json
// import io.taig.otter.Typescript
// import cats.Applicative

// final class JsonTypescriptRenderer[S[_]: Applicative] extends Renderer[Json, S[Typescript]]:
//   val key = ReferenceConstantRenderer(encoder = KeyPrinter.Quoted)
//   val constant = ConstantTypescriptRenderer(printer = JsonPrimitivePrinter)
//   val enumeration = EnumerationTypescriptRenderer[Json.Primitive](printer = JsonPrimitivePrinter)
//   val union = UnionTypescriptRenderer[Json, S](renderer = this)

//   override def render[A](schema: Json[A]): S[Typescript] = schema match
//     case Json.Collection(self) =>
//       render(self.value.schema.value).map(Typescript.Collection.apply)
//     case Json.Constant(self) => constant.render(schema = self).pure
//     case Json.Dictionary(self) =>
//       (
//         KeyTypescriptRenderer.render(schema = self.value.key.value).pure[S],
//         render(schema = self.value.value.value)
//       ).mapN(Typescript.Record.apply)
//     case Json.Enumeration(self) => enumeration.render(schema = self).pure
//     case Json.Nullable(self) =>
//       self.value.schema.fold(Typescript.Void.pure): schema =>
//         render(schema = schema.value).map(Typescript.Nullable.apply)
//     case Json.Primitive(self) => PrimitiveTypescriptRenderer.render(schema = self).pure
//     case Json.Record(self) =>
//       self.value.fields
//         .map(_.value)
//         .traverse(field => render(field.value.value).tupleLeft(key.render(field.key)))
//         .map(Typescript.Object.apply)
//     case Json.Tuple(self) =>
//       self.value.schemas.traverse(schema => render(schema.value)).map(Typescript.Tuple.apply)
//     case Json.Union(self) =>
//       self.value.schemas.traverse(schema => render(schema.value)).map(Typescript.Union.apply)
