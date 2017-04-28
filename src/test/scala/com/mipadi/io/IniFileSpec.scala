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
                   |""".stripMargin
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
    msg should be ("`LBRACE' expected but a found")
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

  it should "return a value for a given key if they key exists" in {
    val options = Map(
      "user" -> Map(
        "name" -> "Michael Dippery",
        "email" -> "michael@monkey-robot.com"
      ),
      "core" -> Map(
        "excludesfile" -> "~/.config/git/ignore"
      ),
      "apply" -> Map(
        "whitespace" -> "nowarn"
      ),
      "color" -> Map(
        "branch" -> "auto",
        "diff" -> "auto",
        "interactive" -> "auto",
        "status" -> "auto"
      ),
      "diff" -> Map(
        "renames" -> "copies"
      ),
      "diff.json" -> Map(
        "textconv" -> "jq '.'"
      ),
      "instaweb" -> Map(
        "httpd" -> "webrick"
      ),
      "interactive" -> Map(
        "singlekey" -> "true"
      ),
      "fetch" -> Map(
        "prune" -> "true"
      ),
      "pull" -> Map(
        "rebase" -> "true"
      ),
      "push" -> Map(
        "default" -> "simple"
      ),
      "rebase" -> Map(
        "autostash" -> "true"
      ),
      "rerere" -> Map(
        "enabled" -> "true"
      ),
      "pager" -> Map(
        "incoming" -> "false",
        "outgoing" -> "false",
        "who" -> "false"
      ),
      "alias" -> Map(
        "br" -> "branch",
        "ci" -> "commit",
        "co" -> "checkout",
        "cp" -> "cherry-pick",
        "ds" -> "!git --no-pager diff --stat -M -w",
        "ga" -> "log --graph --oneline --decorate --all",
        "gr" -> "log --graph --oneline --decorate",
        "ld" -> "log --pretty=format:\"%C(yellow)%h\\\\ %Cgreen%ad%Creset\\\\ %s\\\\ %Cblue[%an]\" --date=short",
        "lf" -> "log --pretty=format:\"%C(yellow)%h\\\\ %Cgreen%ad%Creset\\\\ %s\\\\ %Cblue[%an]\" --date=relative --name-status",
        "ls" -> "log --pretty=format:\"%C(yellow)%h\\\\ %Cgreen%ad%Creset\\\\ %s\\\\ %Cblue[%an]\" --date=relative",
        "ol" -> "log --oneline --decorate",
        "rb" -> "rebase",
        "ri" -> "rebase -i",
        "rp" -> "\"!f() { for remote in $(git remote); do git remote prune $remote; done; }; f\"",
        "rv" -> "revert",
        "ru" -> "remote update",
        "sb" -> "status -sb",
        "st" -> "status",
        "blc" -> "blame-color",
        "alias" -> "\"!f() { git config --get-regexp alias | cut -c 7- | sed \\\"s/ /$(echo 2B | xxd -r -p)/\\\" | column -t -s $(echo 2B | xxd -r -p); }; f\"",
        "authors" -> "!sh -c 'git log --format=\"%aN\" | sort -k 2 -u'",
        "changed" -> "diff ORIG_HEAD..",
        "deleted" -> "log --diff-filter=D --summary",
        "edits" -> "diff --color-words",
        "fix" -> "commit -a --amend",
        "hide" -> "update-index --assume-unchanged",
        "incoming" -> "log --pretty=format:\"%C(yellow)%h\\\\ %Cgreen%ad%Creset\\\\ %s\\\\ %Cblue[%an]%n\" --date=relative ORIG_HEAD..",
        "info" -> "config --list",
        "nuke" -> "!sh -c 'git reset --hard HEAD~$1' -",
        "outgoing" -> "log --pretty=format:\"%C(yellow)%h\\\\ %Cgreen%ad%Creset\\\\ %s\\\\ %Cblue[%an]%n\" --date=relative @{u}..",
        "showtag" -> "!sh -c 'git rev-parse $1 | xargs git cat-file -p' -",
        "staged" -> "diff --cached",
        "stashed" -> "!git --no-pager stash list",
        "tags" -> "tag -n1 -l",
        "trash" -> "!git reset HEAD . && git checkout -- . && echo 'Undid all changes'",
        "type" -> "cat-file -t",
        "undo" -> "reset HEAD .",
        "who" -> "shortlog -sn --no-merges",
        "whoami" -> "!git config --get user.name && git config --get user.email"
      ),
      "include" -> Map(
        "path" -> "~/.gitconfig.user"
      )
    )
    options.foreach { (kv) =>
      val sectionName = kv._1
      val config = kv._2
      val section = complexFile.right.get(sectionName)
      config.foreach { (kv) =>
        val option = kv._1
        val value = kv._2
        val actualValue = section(option) getOrElse "<None>"
        actualValue should be (value)
      }
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
    msg should be ("`NEWLINE' expected but s found")
  }

  // Empty .ini files
  // --------------------------------------------------------------------------

  "An empty .ini file" should "not be parseable" in {
    emptyFile shouldBe a [Left[String, _]]
    val msg = emptyFile.left getOrElse "<Msg>"
    msg should be ("string matching regex `.' expected but end of source found")
  }

  // Valueless .ini files
  // --------------------------------------------------------------------------

  "A .ini file with a key without a value" should "not be parseable" in {
    valuelessFile shouldBe a [Left[String, _]]
    val msg = valuelessFile.left getOrElse "<Msg>"
    msg should be ("`EQUALS' expected but NEWLINE found")
  }
}
