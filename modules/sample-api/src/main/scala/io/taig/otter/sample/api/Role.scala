// package io.taig.otter.sample.api

// import cats.implicits.*
// import cats.kernel.Eq
// import io.taig.otter.sample.api.Role.Guest
// import io.taig.otter.sample.api.schema.LibrarianApiSchema
// import io.taig.otter.sample.api.schema.MemberApiSchema
// import io.taig.otter.sample.api.schema.UserApiSchema

// enum Role:
//   case Guest
//   case Librarian
//   case Member
//   case Or[A <: Role, B <: Role](a: A, b: B)

//   def ^(role: Role): this.type ^ role.type = Or(this, role)

//   def toSet: Set[Role.Guest | Role.Librarian | Role.Member] = this match
//     case Guest     => Set(Guest)
//     case Librarian => Set(Librarian)
//     case Member    => Set(Member)
//     case Or(a, b)  => a.toSet ++ b.toSet

// object Role:
//   type Guest = Role.Guest.type
//   type Librarian = Role.Librarian.type
//   type Member = Role.Member.type

//   def from(user: UserApiSchema): Role.Librarian | Role.Member = user match
//     case _: LibrarianApiSchema => Role.Librarian
//     case _: MemberApiSchema    => Role.Member

//   given [A <: Role]: Eq[A] = Eq.fromUniversalEquals

// type ^[A <: Role, B <: Role] = Role.Or[A, B]
