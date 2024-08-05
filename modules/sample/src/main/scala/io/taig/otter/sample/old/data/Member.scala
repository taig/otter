// package io.taig.otter.sample.data

// import cats.Eq
// import cats.syntax.all.*
// import io.taig.otter.sample.Dsl.*
// import org.typelevel.ci.CIString

// import java.util.regex.Pattern

// final case class Member(
//     reference: Member.Reference,
//     email: Member.Email,
//     password: Member.Password,
//     session: Option[Session]
// ):
//   def toSummary: Member.Summary = Member.Summary(reference, email)

// object Member:
//   opaque type Reference = CIString
//   object Reference:
//     val Length = 8
//     extension (self: Member.Reference) def toCIString: CIString = self
//     def unsafe(value: CIString): Member.Reference = value
//     // val validation: CodecValidation.Primitive[String, Member.Reference] = length(Length).tap.map(CIString.apply)
//     given (using eq: Eq[CIString]): Eq[Member.Reference] = eq

//   opaque type Email = CIString
//   object Email:
//     extension (self: Member.Email) def toCIString: CIString = self
//     def unsafe(value: CIString): Member.Email = value
//     // val validation: CodecValidation.Primitive[String, Member.Email] =
//     //   val pattern = Pattern.compile(".+@.+")
//     //   matches(pattern).tap.map(CIString.apply)

//     given (using eq: Eq[CIString]): Eq[Member.Email] = eq

//   opaque type Password = String
//   // object Password:
//   //   def unsafe(value: String): Member.Password = value
//   //   val validation: CodecValidation.Primitive[String, Member.Password] = minLength(6).tap

//   final case class Login(email: CIString, password: String)

//   final case class Create(email: Member.Email, password: Member.Password)

//   final case class Summary(reference: Member.Reference, email: Member.Email)
