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

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}


sealed trait Token

case object LBRACE extends Token
case object RBRACE extends Token
case object EQUALS extends Token
case object NEWLINE extends Token
case class QUOTED(s: String) extends Token
case class STRING(s: String) extends Token


sealed trait Ast
case class SectionHeader(name: String) extends Ast
case class KeyValuePair(key: String, value: String) extends Ast


trait IniParseError

case class LexerError(msg: String) extends IniParseError


object IniLexer extends RegexParsers {
  override val whiteSpace = "[ \t\r\f]+".r

  override def skipWhitespace = true

  def lbrace  = "\\[".r ^^ { _ => LBRACE }
  def rbrace  = "\\]".r ^^ { _ => RBRACE }
  def equals  = "=".r   ^^ { _ => EQUALS }
  def newline = "\\n".r ^^ { _ => NEWLINE }

  def quoted: Parser[QUOTED] =
    "\".+\"".r ^^ { str => QUOTED(str.replace("\"", "")) }

  def string: Parser[STRING] =
    "[^\\[\\]\\n\\t\\r\\f =]+".r ^^ { str => STRING(str) }

  def tokens: Parser[List[Token]] =
    phrase(rep1(lbrace | rbrace | equals | newline | quoted | string)) ^^ { rawTokens =>
      rawTokens
    }

  def apply(code: String): Either[LexerError, List[Token]] = parse(tokens, code) match {
    case NoSuccess(msg, next) => Left(LexerError(msg))
    case Success(res, next)   => Right(res)
  }
}


class IniTokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Token] = new IniTokenReader(tokens.tail)
}


class IniParser extends Parsers {
  override type Elem = Token

  private def string: Parser[STRING] =
    accept("string", { case str @ STRING(s) => str })

  def sectionHeader: Parser[SectionHeader] =
    (LBRACE ~ string ~ RBRACE ~ rep1(NEWLINE)) ^^ {
      case _ ~ STRING(s) ~ _ ~ _ => SectionHeader(s)
    }

  def keyValuePair: Parser[KeyValuePair] =
    (string ~ EQUALS ~ string ~ rep1(NEWLINE)) ^^ {
      case STRING(k) ~ _ ~ STRING(v) ~ _ => KeyValuePair(k, v)
    }
}
