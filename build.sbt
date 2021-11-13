import sbt.util
name := "caio"

organization := "io.higherstate"

// sbt-release plugin settings

releaseUseGlobalVersion := false

// Replace 'publish' step with 'publishLocal' step
releaseProcess -= ReleaseTransformations.publishArtifacts

// end sbt-release plugin settings

// enable publishing the jar produced by `test:package`
Test / packageBin / publishArtifact := true

// enable publishing the test API jar
Test / packageDoc / publishArtifact := true

// enable publishing the test sources jar
Test / packageSrc / publishArtifact := true

ThisBuild / evictionErrorLevel := util.Level.Warn

val currentScalaVersion = "2.13.7"
ThisBuild / scalaVersion := currentScalaVersion

scalacOptions ++= Seq(
  "-deprecation",                  // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8",                         // Specify character encoding used by source files.
  "-explaintypes",                 // Explain type errors in more detail.
  "-feature",                      // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials",        // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds",         // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked",                    // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit",                   // Wrap field accessors to throw an exception on uninitialized access.
  "-Xlint:adapted-args",           // Warn if an argument list is modified to match the receiver.
  "-Xlint:constant",               // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select",     // Selecting member of DelayedInit.
  "-Xlint:doc-detached",           // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible",           // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any",              // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator",   // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-unit",           // Warn when nullary methods return Unit.
  "-Xlint:option-implicit",        // Option.apply used implicit view.
  "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow",         // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align",            // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow",  // A local type parameter shadows a type already in scope.
  "-Ywarn-dead-code",              // Warn when dead code is identified.
  "-Ywarn-extra-implicit",         // Warn when more than one implicit parameter section is defined.
  "-Ywarn-numeric-widen",          // Warn when numerics are widened.
  "-Ywarn-unused:imports",         // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",          // Warn if a local definition is unused.
  "-Ywarn-unused:patvars",         // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates",        // Warn if a private member is unused.
  "-Ywarn-value-discard",           // Warn when non-Unit expression results are unused.
  "-Ymacro-annotations"
)

javacOptions ++= Seq("-target", "1.11", "-source", "1.11", "-Xlint:deprecation")

libraryDependencies ++= Seq(
  "org.typelevel"        %% "cats-mtl"         % "1.2.1",
  "org.typelevel"        %% "cats-effect"      % "3.2.9",
  "org.typelevel"        %% "cats-mtl-laws"    % "1.2.1" % "test",
  "org.typelevel"        %% "cats-effect-laws" % "3.2.9" % "test",
  "org.typelevel"        %% "discipline-munit" % "1.0.6" % "test",
  "com.github.alterego7" %% "alphabet-soup"    % "0.4.0",
  "org.scalatest"        %% "scalatest"        % "3.0.8" % "test"
)

testFrameworks += new TestFramework("munit.Framework")

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)

resolvers ++= Seq(
  "Maven Central Server" at "https://repo1.maven.org/maven2",
  "Typesafe Releases" at "https://repo.typesafe.com/typesafe/releases/",
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
