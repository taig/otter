// package io.taig.otter.codec

// import cats.Applicative
// import cats.data.Chain
// import cats.syntax.all.*
// import io.taig.otter.Tuple
// import io.taig.otter.Typescript

// final class TupleTypescriptRenderer[S[_], T[_]: Applicative, A](renderer: Renderer[S, T[A]])
//     extends Renderer[Tuple[S, *], T[Typescript[A]]]:
//   override def render[B](schema: Tuple[S, B]): T[Typescript[A]] = schema.value.schemas
//     .traverse(schema => renderer.render(schema = schema.value))
//     .map(Typescript.Tuple.apply)
