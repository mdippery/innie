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

// With guidance from: <http://enear.github.io/2016/03/31/parser-combinators/>

package com.mipadi.lang.ini

import scala.util.parsing.combinator._
import scala.util.parsing.input.{NoPosition, Position, Reader}


private[ini] sealed trait Token
private[ini] case object LBRACE extends Token
private[ini] case object RBRACE extends Token
private[ini] case object EQUALS extends Token
private[ini] case object NEWLINE extends Token
private[ini] case class QUOTED(s: String) extends Token
private[ini] case class STRING(s: String) extends Token


private[ini] sealed trait Ast
private[ini] case class Section(header: SectionHeader, settings: Seq[KeyValuePair]) extends Ast
private[ini] case class SectionHeader(name: String) extends Ast
private[ini] case class KeyValuePair(key: String, value: String) extends Ast


private[ini] trait IniParseError { def msg: String }
private[ini] case class LexerError(msg: String) extends IniParseError
private[ini] case class ParserError(msg: String) extends IniParseError


private[ini] object IniLexer extends RegexParsers {
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


private[ini] class IniTokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Token] = new IniTokenReader(tokens.tail)
}


private[ini] object IniParser extends Parsers {
  override type Elem = Token

  private def string: Parser[STRING] =
    accept("string", { case str @ STRING(s) => str })

  private def quoted: Parser[QUOTED] =
    accept("string", { case str @ QUOTED(s) => str })

  def document: Parser[List[Section]] =
    rep1(section) ^^ { case blocks => blocks }

  def section: Parser[Section] =
    (sectionHeader ~ rep(keyValuePair)) ^^ {
      case h ~ pairs => Section(h, pairs)
    }

  def sectionHeader: Parser[SectionHeader] =
    (LBRACE ~ string ~ opt(quoted) ~ RBRACE ~ rep1(NEWLINE)) ^^ {
      case _ ~ STRING(s1) ~ Some(QUOTED(s2)) ~ _ ~ _ => SectionHeader(s"$s1.$s2")
      case _ ~ STRING(s) ~ None ~ _ ~ _              => SectionHeader(s)
    }

  def keyValuePair: Parser[KeyValuePair] =
    (string ~ EQUALS ~ string ~ rep1(NEWLINE)) ^^ {
      case STRING(k) ~ _ ~ STRING(v) ~ _ => KeyValuePair(k, v)
    }

  def apply(tokens: Seq[Token]): Either[ParserError, List[Section]] = {
    val reader = new IniTokenReader(tokens)
    document(reader) match {
      case NoSuccess(msg, next) => Left(ParserError(msg))
      case Success(res, next)   => Right(res)
    }
  }
}


private[ini] object IniProcessor {
  def apply(code: String): Either[IniParseError, List[Section]] = {
    for {
      tokens <- IniLexer(code).right
      ast <- IniParser(tokens).right
    } yield ast
  }
}
