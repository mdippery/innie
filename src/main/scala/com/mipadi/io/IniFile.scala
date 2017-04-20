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


class IniFile private(_path: String) {
  type IniSection = Map[String,String]

  val path = _path

  def apply(key: String): Option[IniSection] = key match {
    // TODO: Actually parse file and build Map
    case "database" => Some(Map())
    case _          => None
  }
}

object IniFile {
  def apply(path: String): Option[IniFile] = apply(new File(path))

  def apply(file: File): Option[IniFile] =
    if (file.exists) Option(new IniFile(file.getAbsolutePath)) else None
}
