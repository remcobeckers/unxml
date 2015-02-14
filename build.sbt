import scalariform.formatter.preferences._
import com.scalapenos.sbt.prompt.SbtPrompt.autoImport._

lazy val projectSettings = Seq(
  organization := "org.rembo",
  scalaVersion := "2.11.5",
  version := "0.0.3",
  name := "unxml",
  licenses += ("Apache-2.0", url("http://opensource.org/licenses/apache2.0")),
  isSnapshot := true
)

lazy val scalaSettings = Seq(
  scalacOptions := Seq("-encoding", "utf8",
    "-target:jvm-1.7",
    "-feature",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-unchecked",
    "-deprecation",
    "-Xlog-reflective-calls",
    "-Yrangepos"
  ),
  libraryDependencies ++= {
    val akkaVersion = "2.3.9"
    Seq(
      "org.scala-lang.modules" %% "scala-xml"                     % "1.0.2",
      "com.fasterxml"           % "aalto-xml"                     % "0.9.9",
      "com.typesafe.akka"       % "akka-stream-experimental_2.11" % "1.0-M3",
      "org.scalatest"          %% "scalatest"                     % "2.2.1" % "test",
      "com.typesafe.akka"      %% "akka-testkit"                  % akkaVersion  % "test",
    "com.github.nscala-time"   %% "nscala-time"                   % "1.6.0" % "test"
    )
  }
)

lazy val root = project.in(file("."))
  .settings(scalaSettings:_*)
  .settings(projectSettings:_*)
  .settings(Boilerplate.settings: _*)
  .settings(promptTheme := Scalapenos)
  .settings(bintrayPublishSettings:_*)
  .settings(publishMavenStyle := true)
  .settings(scalariformSettings: _*)
  .settings(ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(PreserveDanglingCloseParenthesis, true)
    .setPreference(RewriteArrowSymbols, true)
    .setPreference(AlignParameters, false)
    .setPreference(DoubleIndentClassDeclaration, true)
    .setPreference(AlignSingleLineCaseStatements.MaxArrowIndent, 90)
  )
