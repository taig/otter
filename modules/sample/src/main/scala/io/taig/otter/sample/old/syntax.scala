// package io.taig.otter.sample

// import io.taig.otter.sample.api.Role
// import io.taig.otter.sample.data.{Librarian, Member}

// trait syntax:
//   extension (self: User)
//     def role: Role.Librarian | Role.Member = self match
//       case _: Librarian.Summary => Role.Librarian
//       case _: Member            => Role.Member

// object syntax extends syntax
