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

package com.mipadi.io.helpers

import org.scalatest._
import com.mipadi.io.helpers._


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
}
