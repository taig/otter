// package io.taig.otter.sample

// import cats.effect.{IO, Resource}
// import org.http4s.HttpApp
// import org.http4s.ember.server.EmberServerBuilder
// import org.http4s.server.Server
// import org.http4s.server.middleware.CORS
// import org.typelevel.log4cats.LoggerFactory
// import org.typelevel.log4cats.slf4j.Slf4jFactory

// object SampleServer:
//   def apply(app: HttpApp[IO]): Resource[IO, Server] =
//     given LoggerFactory[IO] = Slf4jFactory.create[IO]

//     Resource
//       .eval(CORS.policy.withAllowOriginAll(app))
//       .flatMap(app => EmberServerBuilder.default[IO].withHttpApp(app).build)
