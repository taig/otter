// package io.taig.otter.sample.api.codecs

// import io.taig.otter.{Primitive, Record}
// import io.taig.otter.dsl.*
// import io.taig.otter.sample.data.Member

// object member:
//   val reference: Primitive.Required[Member.Reference] = cistring.ivalidate(Member.Reference.validation)(_.toCIString)
//   val email: Primitive.Required[Member.Email] = cistring.ivalidate(Member.Email.validation)(_.toCIString)
//   val password: Primitive.Required[Member.Password] = string.ivalidate(Member.Password.validation)(_.toString)

//   val login: Record[Member.Login] = (field("email", cistring) :* field("password", string))
//     .to[Member.Login]
//     .name("Member.Login")

//   val create: Record[Member.Create] = (field("email", email) :* field("password", password))
//     .to[Member.Create]
//     .name("Member.Create")

//   val summary: Record[Member.Summary] = (field("reference", reference) :* field("email", email))
//     .to[Member.Summary]
//     .name("Member.Summary")
