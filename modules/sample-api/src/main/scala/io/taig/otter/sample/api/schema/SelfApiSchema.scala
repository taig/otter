// package io.taig.otter.sample.api.schema

// import io.taig.otter.sample.api.Role
// import io.taig.otter.sample.api.^

// type SelfApiSchema[R <: Role] = R match
//   case Role.Guest     => Unit
//   case Role.Member    => MemberApiSchema
//   case Role.Librarian => LibrarianApiSchema
//   case Role.Guest ^ a => Option[SelfApiSchema[a]]
//   case a ^ Role.Guest => Option[SelfApiSchema[a]]
//   case a ^ b          => SelfApiSchema[a] | SelfApiSchema[b]
