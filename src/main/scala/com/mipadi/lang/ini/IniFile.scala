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


trait IniSection {
  def apply(key: String): Option[String]
}

private[ini] object IniSection {
  def apply(ast: Section): IniSection = new ConcreteIniSection(ast.settings.foldLeft(Map[String, String]()) { (memo, kv) =>
    memo + (kv.key -> kv.value)
  })
}


private[ini] class ConcreteIniSection(_settings: Map[String, String]) extends IniSection {
  override def toString = s"ConcreteIniSection(${_settings})"

  def apply(key: String): Option[String] = _settings get key
}


private[ini] class ProxyIniSection extends IniSection {
  override def toString = "ProxyIniSection"

  def apply(key: String): Option[String] = None
}


class IniFile private(_path: String, _sections: Map[String, IniSection]) {
  val path = _path

  override def toString = s"IniFile(path = $path)"

  def apply(key: String): IniSection =
    _sections get key getOrElse new ProxyIniSection()
}

object IniFile {
  def apply(path: String): Either[String, IniFile] = apply(new File(path))

  def apply(file: File): Either[String, IniFile] = if (file.exists) {
    IniProcessor(Source.fromFile(file).mkString) match {
      case Right(sections) =>
        val sects = sections.foldLeft(Map[String, IniSection]()) { (memo, section) =>
          val name = section.header.name
          val data = IniSection(section)
          memo + (name -> data)
        }
        Right(new IniFile(file.getAbsolutePath, sects))

      case Left(err) =>
        Left(err.msg)
    }
  } else {
    Left(s"Cannot stat file: ${file.getPath}")
  }
}
