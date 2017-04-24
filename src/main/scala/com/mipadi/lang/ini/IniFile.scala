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

package com.mipadi.lang.ini

import java.io.File
import scala.io.Source
import com.mipadi.lang.ini.helpers._


class IniSection private[ini](_settings: Map[String, String]) {
  override def toString = s"IniSection(${_settings})"
}


class IniFile private(_path: String, _sections: Map[String, IniSection]) {
  val path = _path

  override def toString = s"IniFile(path = $path)"

  def apply(key: String): Option[IniSection] = _sections get key
}

object IniFile {
  def apply(path: String): Either[String, IniFile] = apply(new File(path))

  def apply(file: File): Either[String, IniFile] = if (file.exists) {
    val sections = Source.fromFile(file).getLines.foldLeft(Map[String, IniSection]()) { (memo, e) =>
      if (e.isSectionName) {
        memo + (e.cleanSectionName -> new IniSection(Map[String, String]()))
      } else {
        memo
      }
    }
    Right(new IniFile(file.getAbsolutePath, sections))
  } else {
    Left(s"Cannot stat file: ${file.getPath}")
  }
}
