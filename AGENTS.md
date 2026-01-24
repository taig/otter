# Otter

Extensible schema definition library for serialization formats (e.g. JSON, XML and CSV) with self-documenting API definition capabilities.

## Devlopment workflow

### Fast loop

Stay in one project.

```
# Compile to check for errors quickly
sbt core-json-typescript-effect/compile

# Run tests for current project
sbt core-json-typescript-effect/test
sbt "core-json-typescript-effect/testOnly io.taig.otter.codec.JsonTypescriptEffectRendererTest"
```

## Code Style

- Scalafmt enforced (maxColumn: 120, Scala 3 dialect)
- No comments explaining obvious code changes
- Follow existing patterns in neighboring files

## Boundaries

### Always do

- Compile and run tests after modifying code
- Apply Scalafmt by running `sbt scalafmtAll`

### Ask first

- Adding new dependencies (even test-only)
- Creating new subprojects
- Changing build configuration
- Modifying CI workflows

### Never do

- Add dependencies
- Commit without formatting
- Delete or skip tests to make CI pass