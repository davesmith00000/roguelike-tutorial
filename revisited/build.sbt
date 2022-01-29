import scala.sys.process._
import scala.language.postfixOps

import sbtwelcome._

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0"

lazy val roguelike =
  (project in file("."))
    .enablePlugins(ScalaJSPlugin, SbtIndigo)
    .settings(
      name         := "roguelike",
      version      := "0.0.1",
      scalaVersion := "3.1.0",
      organization := "roguelike",
      libraryDependencies ++= Seq(
        "org.scalameta" %%% "munit" % "0.7.29" % Test
      ),
      testFrameworks += new TestFramework("munit.Framework"),
      Test / scalaJSLinkerConfig ~= {
        _.withModuleKind(ModuleKind.CommonJSModule)
      },
      showCursor          := true,
      title               := "Indigo Roguelike!",
      gameAssetsDirectory := "assets",
      windowStartWidth    := 80 * 10,
      windowStartHeight   := 50 * 10,
      libraryDependencies ++= Seq(
        "io.indigoengine" %%% "indigo-json-circe"    % "0.11.0",
        "io.indigoengine" %%% "indigo"               % "0.11.0",
        "io.indigoengine" %%% "indigo-extras"        % "0.11.0",
        "io.indigoengine" %%% "roguelike-starterkit" % "0.1.0-SNAPSHOT"
      ),
      scalafixOnCompile := true,
      semanticdbEnabled := true,
      semanticdbVersion := scalafixSemanticdb.revision
      // scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) } // required for parcel, but will break indigoRun & indigoBuild
    )
    .settings(
      code := {
        val command = Seq("code", ".")
        val run = sys.props("os.name").toLowerCase match {
          case x if x contains "windows" => Seq("cmd", "/C") ++ command
          case _                         => command
        }
        run.!
      }
    )
    .settings(
      logo := rawLogo + "(v" + version.value.toString + ")",
      usefulTasks := Seq(
        UsefulTask("a", "runGame", "Run the game (requires Electron)"),
        UsefulTask("b", "buildGame", "Build web version"),
        UsefulTask(
          "c",
          "runGameFull",
          "Run the fully optimised game (requires Electron)"
        ),
        UsefulTask(
          "d",
          "buildGameFull",
          "Build the fully optimised web version"
        ),
        UsefulTask("e", "code", "Launch VSCode")
      ),
      logoColor        := scala.Console.YELLOW,
      aliasColor       := scala.Console.BLUE,
      commandColor     := scala.Console.CYAN,
      descriptionColor := scala.Console.WHITE
    )

// To use indigoBuild or indigoRun, first comment out the line above that says: `scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }`
addCommandAlias("runGame", ";compile;fastOptJS;indigoRun")
addCommandAlias("runGameFull", ";compile;fullOptJS;indigoRunFull")
addCommandAlias("buildGame", ";compile;fastOptJS;indigoBuild")
addCommandAlias("buildGameFull", ";compile;fullOptJS;indigoBuildFull")

lazy val code =
  taskKey[Unit]("Launch VSCode in the current directory")

// format: off
lazy val rawLogo: String =
"""
 ____   __    ___  _  _  ____  __    __  __ _  ____ 
(  _ \ /  \  / __)/ )( \(  __)(  )  (  )(  / )(  __)
 )   /(  O )( (_ \) \/ ( ) _) / (_/\ )(  )  (  ) _) 
(__\_) \__/  \___/\____/(____)\____/(__)(__\_)(____)
"""
