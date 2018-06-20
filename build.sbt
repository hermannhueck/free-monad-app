import sbt.Resolver

name := "playing-with-free-monad"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.6"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",   // source files are in UTF-8
  "-deprecation",         // warn about use of deprecated APIs
  "-unchecked",           // warn about unchecked type parameters
  "-feature",             // warn about misused language features
  //"-Xlint",               // enable handy linter warnings
  "-Ypartial-unification" // allow the compiler to unify type constructors of different arities
  //"-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  // "-Xfatal-warnings",     // turn compiler warnings into errors
)

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.bintrayRepo("projectseptemberinc", "maven")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.1.0",
  "org.typelevel" %% "cats-free" % "1.1.0",
  "com.projectseptember" %% "freek" % "0.6.7"
  // "com.milessabin" %% "si2712fix-plugin" % "1.2.0"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
