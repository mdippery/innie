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
import com.mipadi.lang.ini.IniSection


class IniFileSpec extends FlatSpec with Matchers {
  val iniFile = IniFile("src/test/resources/zanegort.ini")
  val quotedFile = IniFile("src/test/resources/quoted.ini")
  val complexFile = IniFile("src/test/resources/gitconfig.ini")

  // Simple .ini files
  // --------------------------------------------------------------------------

  "A simple .ini file" should "be created from a string path" in {
    val path = iniFile map { _.path } getOrElse "<Left>"
    iniFile shouldBe a [Right[_, IniFile]]
    path.endsWith("src/test/resources/zanegort.ini") should be (true)
  }

  it should "be created from a File" in {
    val f = new File("src/test/resources/zanegort.ini")
    val iniFile = IniFile(f)
    val path = iniFile map { _.path } getOrElse "<Left>"
    iniFile shouldBe a [Right[_, IniFile]]
    path.endsWith("src/test/resources/zanegort.ini") should be (true)
  }

  it should "not be created from an invalid string path" in {
    val f = "src/test/resources/nofile.ini"
    val iniFile = IniFile(f)
    val msg = iniFile.left getOrElse "<Right>"
    iniFile shouldBe a [Left[String, _]]
    msg should be (s"Cannot stat file: $f")
  }

  it should "not be created from an invalid File" in {
    val fn = "src/test/resources/nofile.ini"
    val f = new File(fn)
    val iniFile = IniFile(f)
    val msg = iniFile.left getOrElse "<Right>"
    iniFile shouldBe a [Left[String, _]]
    msg should be (s"Cannot stat file: $fn")
  }

  it should "return a section if a given key is valid" in {
    val sections = Set("database", "irc")
    val file = iniFile.right.get
    sections.foreach { key =>
      val section = file(key)
      section should not be (None)
    }
  }

  it should "not return a section if a given key is invalid" in {
    val file = iniFile.right.get
    val section = file("nickname")
    section should be (None)
  }

  it should "return a value for a given key if the key exists" in {
    val options = Map(
      "database" -> Map(
        "name" -> "zanegort",
        "collection" -> "services"
      ),
      "irc" -> Map(
        "network" -> "freenode",
        "host" -> "chat.freenode.net",
        "port" -> "6667",
        "nickname" -> "zanegort",
        "channel" -> "#/r/webdev"
      )
    )
    options.foreach { (kv) =>
      val sectionName = kv._1
      val config = kv._2
      val section = iniFile.right.get(sectionName)
      section shouldBe a [Some[_]]
      config.foreach { (kv) =>
        val option = kv._1
        val value = kv._2
        val actualValue = section flatMap { _(option) } getOrElse "<None>"
        actualValue should be (value)
      }
    }
  }

  it should "not return a value for a given key if the key does not exist" in {
    val section = iniFile.right.get("database")
    section shouldBe a [Some[_]]
    val value = section flatMap { _("table") } getOrElse "<None>"
    value should be ("<None>")
  }

  // Quoted .ini files
  // --------------------------------------------------------------------------

  "A quoted .ini file" should "return a quoted section if the key is valid" in {
    val file = quotedFile.right.get
    val section = file("section.quoted")
    section should not be (None)
  }

  // Complex .ini files
  // --------------------------------------------------------------------------

  "A complex .ini file" should "be created from a string path" in {
    val complexFile = IniFile("src/test/resources/gitconfig.ini")
    val path = complexFile map { _.path } getOrElse "<Left>"
    path.endsWith("src/test/resources/gitconfig.ini") should be (true)
  }

  it should "be created from a File" in {
    val f = new File("src/test/resources/gitconfig.ini")
    val complexFile = IniFile(f)
    val path = complexFile map { _.path } getOrElse "<Left>"
    path.endsWith("src/test/resources/gitconfig.ini") should be (true)
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
