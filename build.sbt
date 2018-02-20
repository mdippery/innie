/*
 * Copyright (C) 2017 Michael Dippery <michael@monkey-robot.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

lazy val root = (project in file("."))
  .settings(
    name         := "innie",
    organization := "com.mipadi",
    licenses     := Seq(("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0"))),
    version      := "0.1.1-SNAPSHOT",
    scalaVersion := "2.12.4",

    crossScalaVersions := Seq("2.12.4", "2.11.11", "2.10.6"),

    scalacOptions ++= Seq(
      "-deprecation"
    ),

    libraryDependencies := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, scalaMajor)) if scalaMajor > 10 =>
          libraryDependencies.value ++ Seq(
            "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
          )
        case _ =>
          libraryDependencies.value
      }
    },

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.4" % Test
    )
  )
