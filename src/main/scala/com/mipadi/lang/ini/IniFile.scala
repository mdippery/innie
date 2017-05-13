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


/** Represents a single section of a `.ini` file. */
trait IniSection {

  /** Returns the value for a given key in this INI section.
   *
   *  @param key
   *    The key
   *  @return
   *    The value for `key`, wrapped in an `Option`, or `None` if the key
   *    does not exist in this section.
   */
  def apply(key: String): Option[String]
}

private[ini] object IniSection {
  def apply(ast: Section): IniSection =
    new ConcreteIniSection(ast.settings.foldLeft(Map[String, String]()) { (memo, kv) =>
      memo + (kv.key.s -> kv.value.s)
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


/** Represents a `.ini` file. */
class IniFile private(_sections: Map[String, IniSection]) {
  override def toString = s"IniFile(_sections = ${_sections})"

  /** Retrieves a named section of a `.ini` file.
   *
   *  @param key
   *    The name of the section
   *  @return
   *    The section for the given key
   */
  def apply(key: String): IniSection =
    _sections get key getOrElse new ProxyIniSection()
}

/** Builds an `[[com.mipadi.lang.ini.IniFile IniFile]]` object from the given
 *  source code or `File`.
 *
 *  Typically a new `[[com.mipadi.lang.ini.IniFile IniFile]]` object is
 *  created from a `java.io.File`:
 *
 *  {{{
 *  import java.io.File
 *  import com.mipadi.lang.ini.IniFile
 *  val f = new File("/path/to/file.ini")
 *  val ini = IniFile(f)
 *  }}}
 */
object IniFile {

  /** Creates a new `[[com.mipadi.lang.ini.IniFile IniFile]]` from the given
   *  source code (as a string).
   *
   *  @param code
   *    `.ini` source code
   *  @return
   *    - A `Right` containing the processed `.ini` file; or
   *    - A `Left` containing an error message if the INI source is not
   *      valid.
   */
  def apply(code: String): Either[String, IniFile] = IniProcessor(code) match {
    case Right(sections) =>
      val sects = sections.foldLeft(Map[String, IniSection]()) { (memo, section) =>
        val name = section.header.name
        val data = IniSection(section)
        memo + (name -> data)
      }
      Right(new IniFile(sects))

    case Left(err) =>
      Left(err.msg)
  }

  /** Creates a new `[[com.mipadi.lang.ini.IniFile IniFile]]` from the given
   *  file object.
   *
   *  @param file
   *    The `.ini` file
   *  @return
   *    - A `Right` containing the processed `.ini` file; or
   *    - A `Left` containing an error message if the file cannot be
   *      processed.
   */
  def apply(file: File): Either[String, IniFile] = if (file.exists) {
    apply(Source.fromFile(file).mkString)
  } else {
    Left(s"Cannot stat file: ${file.getPath}")
  }
}
