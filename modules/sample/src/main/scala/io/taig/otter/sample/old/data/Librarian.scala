// package io.taig.otter.sample.data

// import cats.Eq
// import cats.syntax.all.*
// import io.taig.otter.sample.Dsl.*
// import org.typelevel.ci.*

// import java.util.regex.Pattern
// import java.util.UUID

// final case class Librarian(
//     reference: Librarian.Reference,
//     email: Librarian.Email,
//     password: Librarian.Password,
//     session: Option[Session]
// ):
//   def toSummary: Librarian.Summary = Librarian.Summary(reference, email)

// object Librarian:
//   opaque type Reference = UUID
//   object Reference:
//     val Length = 8
//     extension (self: Librarian.Reference) def toUUID: UUID = self
//     def apply(value: UUID): Librarian.Reference = value

//   opaque type Email = CIString
//   object Email:
//     extension (self: Librarian.Email) def toCIString: CIString = self
//     def unsafe(value: CIString): Librarian.Email = value
//     // val validation: CodecValidation.Primitive[String, Librarian.Email] =
//     //   val pattern = Pattern.compile(".+@.+")
//     //   matches(pattern).tap.map(CIString.apply)

//     given (using eq: Eq[CIString]): Eq[Librarian.Email] = eq

//   opaque type Password = String
//   object Password:
//     def unsafe(value: String): Librarian.Password = value
//     // val validation: CodecValidation.Primitive[String, Librarian.Password] = minLength(6).tap

//   final case class Login(email: CIString, password: String)

//   final case class Create(email: Librarian.Email, password: Librarian.Password):
//     def toLogin: Librarian.Login = Login(email.toCIString, password.toString)

//   object Create:
//     val Default: Librarian.Create = Create(
//       Librarian.Email.unsafe(ci"otter@taig.io"),
//       Librarian.Password.unsafe("password")
//     )

//   final case class Summary(reference: Librarian.Reference, email: Librarian.Email)
