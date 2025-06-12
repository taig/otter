// package io.taig.otter.codec

// import cats.data.State
// import cats.syntax.all.*
// import io.taig.otter.Typescript
// import io.taig.otter.Collection
// import io.taig.otter.Constant
// import io.taig.otter.Dictionary
// import io.taig.otter.Record
// import io.taig.otter.Key
// import io.taig.otter.Primitive
// import io.taig.otter.Nullable
// import io.taig.otter.Enumeration
// import io.taig.otter.Tuple
// import io.taig.otter.Union
// import cats.Applicative
// import cats.Order

// final class TypescriptRenderer[S[_], T[_], U[_], V[_]: Applicative, A: Order](
// ) extends Renderer[TypescriptRenderer.Input[S, T, U, *], V[Typescript[A]]]:
//   val collection = CollectionTypescriptRenderer(renderer)

//   val constant = ConstantTypescriptRenderer(printer).map[V[Typescript[A]]](_.pure[V])

//   val dictionary = DictionaryTypescriptRenderer[Key, S, V, A](key = key.map(_.pure[V]), value = renderer)

//   val enumeration = EnumerationTypescriptRenderer(printer).map[V[Typescript[A]]](_.pure[V])

//   val nullable = NullableTypescriptRenderer(renderer)

//   val primitive = PrimitiveTypescriptRenderer.map[V[Typescript[A]]](_.pure[V])

//   val record = RecordTypescriptRenderer(renderer = field)

//   val tuple = TupleTypescriptRenderer(renderer)

//   val union = UnionTypescriptRenderer(renderer)

//   override def render[B](schema: TypescriptRenderer.Input[S, T, U, B]): V[Typescript[A]] = schema match
//     case schema: Collection[S, B]      => collection.render(schema)
//     case schema: Constant[T, B]        => constant.render(schema)
//     case schema: Dictionary[Key, S, B] => dictionary.render(schema)
//     case schema: Enumeration[T, B]     => enumeration.render(schema)
//     case schema: Nullable[S, B]        => nullable.render(schema)
//     case schema: Primitive[S, B]       => primitive.render(schema)
//     case schema: Record[U, B]          => record.render(schema)
//     case schema: Tuple[S, B]           => tuple.render(schema)
//     case schema: Union[S, B]           => union.render(schema)

// object TypescriptRenderer:
//   type Input[S[_], T[_], U[_], A] = Collection[S, A] | Constant[T, A] | Dictionary[Key, S, A] | Enumeration[T, A] |
//     Nullable[S, A] | Primitive[S, A] | Record[U, A] | Tuple[S, A] | Union[S, A]
