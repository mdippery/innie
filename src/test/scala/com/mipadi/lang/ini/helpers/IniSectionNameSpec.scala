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

package com.mipadi.lang.ini.helpers

import org.scalatest._
import com.mipadi.lang.ini.helpers._


class IniSectionNameSpec extends FlatSpec with Matchers {
  "A string" should "be a section name if it is enclosed in brackets" in {
    "[section]".isSectionName should be (true)
  }

  it should "not be a section name if it is not enclosed in brackets" in {
    "section".isSectionName should be (false)
  }

  it should "not be a section name if the braces are not matched" in {
    "]section[".isSectionName should be (false)
  }

  it should "not be a section name if it only starts with a bracket" in {
    "[section".isSectionName should be (false)
    "]section".isSectionName should be (false)
  }

  it should "not be a section name if it only ends with a bracket" in {
    "section]".isSectionName should be (false)
    "section[".isSectionName should be (false)
  }

  it should "remove brackets from section names" in {
    "[section]".cleanSectionName should be ("section")
  }

  it should "not remove brackets from inside a section name" in {
    "sec[tio]n".cleanSectionName should be ("sec[tio]n")
  }

  it should "replace quotation marks in a quoted section name" in {
    "[section \"special\"]".cleanSectionName should be ("section.special")
  }

  it should "split into a key/value pair" in {
    "key = value".splitKeyAndValue should be (Some("key" -> "value"))
  }

  it should "split into a key/value pair if it is missing a leading space" in {
    "key= value".splitKeyAndValue should be (Some("key" -> "value"))
  }

  it should "split into a key/value pair if it is missing a trailing space" in {
    "key =value".splitKeyAndValue should be (Some("key" -> "value"))
  }

  it should "split into a key/value pair if it is missing both spaces" in {
    "key=value".splitKeyAndValue should be (Some("key" -> "value"))
  }

  it should "not split if it is not a key/value pair" in {
    "key value".splitKeyAndValue should be (None)
  }
}
