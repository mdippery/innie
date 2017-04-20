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

  "An IniFile" should "be created from a string path" in {
    val path = iniFile map { _.path } getOrElse ""
    iniFile should not be (None)
    path.endsWith("src/test/resources/zanegort.ini") should be (true)
  }

  it should "be created from a File" in {
    val f = new File("src/test/resources/zanegort.ini")
    val iniFile = IniFile(f)
    val path = iniFile map { _.path } getOrElse ""
    iniFile should not be (None)
    path.endsWith("src/test/resources/zanegort.ini") should be (true)
  }

  it should "not be created from an invalid string path" in {
    val iniFile = IniFile("src/test/resources/nofile.ini")
    iniFile should be (None)
  }

  it should "not be created from an invalid File" in {
    val f = new File("src/test/resources/nofile.ini")
    val iniFile = IniFile(f)
    iniFile should be (None)
  }

  it should "return a section if a given key is valid" in {
    val section = iniFile map { _("database") } getOrElse None
    section should not be (None)
  }

  it should "not return a section if a given key is invalid" in {
    val section = iniFile map { _("nosection") } getOrElse None
    section should be (None)
  }
}