// package io.taig.otter.codec

// import cats.syntax.all.*
// import io.taig.otter.Json
// import io.taig.otter.TypescriptZodState
// import io.taig.otter.syntax.EnrichedSyntax.*
// import io.taig.otter.TypescriptZod
// import io.taig.otter.TypescriptKeys
// import io.taig.otter.TypescriptZodKeys
// import io.taig.otter.Typescript
// import io.taig.otter.TypescriptState
// import io.taig.otter.ContextState
// import cats.data.State
// import scala.collection.immutable.SortedSet

// final class JsonTypescriptMetadataZodRenderer(renderer: Renderer[Json, TypescriptZodState[TypescriptZod]])
//     extends Renderer[Json, TypescriptZodState[TypescriptZod]]:
//   override def render[A](schema: Json[A]): TypescriptZodState[TypescriptZod] =
//     (schema.metadata.get(TypescriptKeys.typescript), schema.metadata.get(TypescriptZodKeys.zod)) match
//       case (Some(typescript), Some(zod)) =>
//         TypescriptZod
//           .Split(
//             typescript = Typescript.Dynamic(typescript),
//             zod = Typescript.Dynamic(typescript)
//           )
//           .pure
//       case (None, Some(zod)) =>
//         renderer
//           .render(schema)
//           .map:
//             case TypescriptZod.Shared(self) => TypescriptZod.Split(typescript = ???, zod = Typescript.Dynamic(zod))
//             case TypescriptZod.Split(typescript, _) => TypescriptZod.Split(typescript, zod = Typescript.Dynamic(zod))
//       case (Some(typescript), None) =>
//         val zod: Renderer[
//           Json,
//           State[ContextState.Context[Typescript[Typescript.Value]], Typescript[Typescript.Value]]
//         ] = ???

//         State: c =>
//           val zodContext: ContextState.Context[Typescript[Typescript.Value]] = c.map(_.toZod)

//           val (context, tsZod) = zod
//             .render(schema)
//             .run(initial = zodContext)
//             .value

//           val mergedContext = context.references.foldLeft(c.references):
//             case (result, (name, zod)) =>
//               result.updatedWith(name):
//                 case Some(shared @ TypescriptZod.Shared(_)) =>
//                   Some(TypescriptZod.Split(typescript = shared.toTypescript, zod))
//                 case Some(TypescriptZod.Split(typescript, _)) => Some(TypescriptZod.Split(typescript, zod))
//                 case None => Some(TypescriptZod(zod))

//           (
//             ContextState.Context(mergedContext, stack = SortedSet.empty),
//             TypescriptZod.Split(typescript = Typescript.Dynamic(typescript), zod = tsZod)
//           )
//       case (None, None) => renderer.render(schema)
