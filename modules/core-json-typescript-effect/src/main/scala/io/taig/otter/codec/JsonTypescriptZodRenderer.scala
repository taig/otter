// package io.taig.otter.codec

// import io.taig.otter.Json
// import io.taig.otter.Typescript

// val JsonTypescriptZodRenderer: Renderer[Json.Write, List[Typescript]] =
//   JsonStateTypescriptExpressionZodRenderer
//     .map(_.runEmpty.value)
//     .map((context, expression) => context.declarations :+ expression)
