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

package com.mipadi.io

import java.io.File
import org.scalatest._


class IniFileSpec extends FlatSpec with Matchers {
  val iniFile = IniFile("src/test/resources/zanegort.ini")
  val quotedFile = IniFile("src/test/resources/quoted.ini")
  val complexFile = IniFile("src/test/resources/gitconfig.ini")

  // Simple .ini files
  // --------------------------------------------------------------------------

  "A simple .ini file" should "be created from a string path" in {
    val iniFile = IniFile("src/test/resources/zanegort.ini")
    iniFile match {
      case Right(f) => f.path.endsWith("src/test/resources/zanegort.ini") should be (true)
      case x        => fail(s"Not a Right: $x")
    }
  }

  it should "be created from a File" in {
    val f = new File("src/test/resources/zanegort.ini")
    val iniFile = IniFile(f)
    iniFile match {
      case Right(f) => f.path.endsWith("src/test/resources/zanegort.ini") should be (true)
      case x        => fail(s"Not a Right: $x")
    }
  }

  it should "not be created from an invalid string path" in {
    val f = "src/test/resources/nofile.ini"
    val iniFile = IniFile(f)
    iniFile match {
      case Left(msg) =>
        msg should be (s"Cannot stat file: $f")
      case x =>
        fail(s"Not a Left: $x")
    }
  }

  it should "not be created from an invalid File" in {
    val fn = "src/test/resources/nofile.ini"
    val f = new File(fn)
    val iniFile = IniFile(f)
    iniFile match {
      case Left(msg) =>
        msg should be (s"Cannot stat file: $fn")
      case x =>
        fail(s"Not a Left: $x")
    }
  }

  it should "return a section if a given key is valid" in {
    val sections = Set("database", "irc")
    iniFile match {
      case Right(f) =>
        sections.foreach { key =>
          val section = f(key)
          section should not be (None)
        }

      case x =>
        fail(s"Not a Right: $x")
    }
  }

  it should "not return a section if a given key is invalid" in {
    iniFile match {
      case Right(f) =>
        val section = f("nickname")
        section should be (None)

      case x =>
        fail(s"Not a Right: $x")
    }
  }

  // Quoted .ini files
  // --------------------------------------------------------------------------

  "A quoted .ini file" should "return a quoted section if the key is valid" in {
    quotedFile match {
      case Right(f) =>
        val section = f("section.quoted")
        section should not be (None)

      case x =>
        fail(s"Not a Right: $x")
    }
  }

  // Complex .ini files
  // --------------------------------------------------------------------------

  "A complex .ini file" should "be created from a string path" in {
    val complexFile = IniFile("src/test/resources/gitconfig.ini")
    complexFile match {
      case Right(f) => f.path.endsWith("src/test/resources/gitconfig.ini") should be (true)
      case x        => fail(s"Not a Right: $x")
    }
  }

  it should "be created from a File" in {
    val f = new File("src/test/resources/gitconfig.ini")
    val complexFile = IniFile(f)
    complexFile match {
      case Right(f) => f.path.endsWith("src/test/resources/gitconfig.ini") should be (true)
      case x        => fail(s"Not a Right: $x")
    }
  }

  /*
  it should "return a section if a given key is valid" in {
    val sections = Set("user", "core", "apply", "color", "diff", "diff.json",
                       "instaweb", "interactive", "fetch", "pull", "push",
                       "rebase", "rerere", "pager", "alias", "include")
    complexFile match {
      case Right(f) =>
        sections.foreach { key =>
          val section = f(key)
          section should not be (None)
        }

      case x =>
        fail(s"Not a Right: $x")
    }
  }

  it should "not return a section if a given key is invalid" in {
    complexFile match {
      case Right(f) =>
        val section = f("autostash")
        section should be (None)

      case x =>
        fail(s"Not a Right: $x")
    }
  }
  */
}
