# openapi (WIP)

Declarative REST endpoint definitions

## Vision

This project currently supports only [http4s](https://github.com/http4s/http4s) backends with [circe](https://github.com/circe/circe) for JSON serialization. Although the library is flexible enough to support other backends and JSON libraries, it is not a goal of this project to provide any alternatives.

- validation
- declarative into domain model, no implicits
- no websocket
- no streaming (?)
- openapi all the way
- scala 3

## Installation

Instructions below transitively include all available modules, which is recommended. To learn more about the available modules, please continue by reading the [modules](#modules) overview. 

### sbt

```scala
libraryDependencies ++=
  "io.hireproof" %% "openapi-schema" % "x.y.z" ::
  "io.hireproof" %% "openapi-http4s" % "x.y.z" ::
  Nil
```

## Getting started

Lorem ipsum

- sample

## Modules

### core  

Lorem ipsum

### validation

Lorem ipsum

### schema

Lorem ipsum

### circe

Lorem ipsum

### http4s

Lorem ipsum

## How does hireproof/openapi differ from …

### … tapir?

Lorem ipsum

### … endpoints4s?

Lorem ipsum

## Acknowledgements

- tapir
- endpoint4s
- skunk
- scodec

## Disclaimer

Lorem ipsum