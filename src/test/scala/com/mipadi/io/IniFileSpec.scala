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
  val iniFile = IniFile(new File("src/test/resources/zanegort.ini"))
  val quotedFile = IniFile(new File("src/test/resources/quoted.ini"))
  val complexFile = IniFile(new File("src/test/resources/gitconfig.ini"))
  val invalidFile = IniFile(new File("src/test/resources/invalid.ini"))
  val emptyFile = IniFile(new File("src/test/resources/empty.ini"))
  val valuelessFile = IniFile(new File("src/test/resources/novalue.ini"))

  // Simple .ini files
  // --------------------------------------------------------------------------

  "A simple .ini file" should "be created from a string" in {
    val source = """[database]
                   |name = zanegort
                   |collection = services
                   |
                   |[irc]
                   |network = freenode
                   |host = chat.freenode.net
                   |port = 6667
                   |nickname = zanegort
                   |channel = #/r/webdev
                   |    val iniFile = IniFile(source)
                   |    iniFile shouldBe a [Right[_, IniFile]]
                   |  }""".stripMargin
    val iniFile = IniFile(source)
    iniFile shouldBe a [Right[_, IniFile]]
  }

  it should "be created from a File" in {
    iniFile shouldBe a [Right[_, IniFile]]
  }

  it should "not be created from an invalid string" in {
    val iniFile = IniFile("aaaaa")
    val msg = iniFile.left getOrElse "<Right>"
    iniFile shouldBe a [Left[String, _]]
    println("`LBRACE' expected but STRING(aaaaa) found")
  }

  it should "not be created from an invalid File" in {
    val fn = "src/test/resources/nofile.ini"
    val f = new File(fn)
    val iniFile = IniFile(f)
    val msg = iniFile.left getOrElse "<Right>"
    iniFile shouldBe a [Left[String, _]]
    msg should be (s"Cannot stat file: $fn")
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
      config.foreach { (kv) =>
        val option = kv._1
        val value = kv._2
        val actualValue = section(option) getOrElse "<None>"
        actualValue should be (value)
      }
    }
  }

  it should "not return a value for a given key if the key does not exist" in {
    val value = iniFile.right.get("database")("table") getOrElse "<None>"
    value should be ("<None>")
  }

  it should "not return a value for a given key if the section does not exist" in {
    val value = iniFile.right.get("nosection")("name") getOrElse "<None>"
    value should be ("<None>")
  }

  // Quoted .ini files
  // --------------------------------------------------------------------------

  "A quoted .ini file" should "return a value from a quoted section if the key is valid" in {
    val file = quotedFile.right.get
    val value = file("section.quoted")("key") getOrElse "<None>"
    value should be ("value")
  }

  it should "not return a quoted section if the section does not exist" in {
    val file = quotedFile.right.get
    val value = file("other.section")("key") getOrElse "<None>"
    value should be ("<None>")
  }

  it should "not return a value in a quote section if the key does not exist" in {
    val file = quotedFile.right.get
    val value = file("section.quoted")("otherkey") getOrElse "<None>"
    value should be ("<None>")
  }

  // Complex .ini files
  // --------------------------------------------------------------------------

  "A complex .ini file" should "be created from a File" in {
    complexFile shouldBe a [Right[_, IniFile]]
  }

  ignore should "return a section if a given key is valid" in {
    val sections = Set("user", "core", "apply", "color", "diff", "diff.json",
                       "instaweb", "interactive", "fetch", "pull", "push",
                       "rebase", "rerere", "pager", "alias", "include")
    val f = complexFile.right.get
    sections.foreach { key =>
      val section = f(key)
      section should not be (None)
    }
  }

  it should "not return a value if a key in a section does not exist" in {
    val value = complexFile.right.get("color")("pull") getOrElse "<None>"
    value should be ("<None>")
  }

  it should "not return a value if a given section does not exist" in {
    val value = complexFile.right.get("autostash")("rebase") getOrElse "<None>"
    value should be ("<None>")
  }

  // Invalid .ini files
  // --------------------------------------------------------------------------

  "An invalid .ini file" should "not be parseable" in {
    invalidFile shouldBe a [Left[String, _]]
    val msg = invalidFile.left getOrElse "<Msg>"
    msg should be ("`NEWLINE' expected but STRING(section) found")
  }

  // Empty .ini files
  // --------------------------------------------------------------------------

  "An empty .ini file" should "not be parseable" in {
    emptyFile shouldBe a [Left[String, _]]
    val msg = emptyFile.left getOrElse "<Msg>"
    msg should be ("string matching regex `[^\\[\\]\\n\\t\\r\\f =]+' expected but end of source found")
  }

  // Valueless .ini files
  // --------------------------------------------------------------------------

  ignore /*"A .ini file with a key without a value"*/ should "not be parseable when it has a key with no value" in {
    valuelessFile shouldBe a [Left[String, _]]
    val msg = valuelessFile.left getOrElse "<Msg>"
    println(msg)
  }
}
