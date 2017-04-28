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
private[ini] case object SPACE extends Token
private[ini] case object DQUOTE extends Token
private[ini] case object NEWLINE extends Token
private[ini] case object EQUALS extends Token
private[ini] case class LETTER(s: String) extends Token { override def toString = s }
private[ini] case class NUMBER(s: String) extends Token { override def toString = s }
private[ini] case class ANY(s: String) extends Token { override def toString = s }


private[ini] sealed trait Ast
private[ini] case class Section(header: SectionHeader, settings: Seq[KeyValuePair]) extends Ast
private[ini] case class SectionHeader(name: String) extends Ast
private[ini] case class Word(s: String) extends Ast { override def toString = s }
private[ini] case class Quoted(s: String) extends Ast { override def toString = s }
private[ini] case class Key(s: String) extends Ast
private[ini] case class Value(s: String) extends Ast
private[ini] case class KeyValuePair(key: Key, value: Value) extends Ast


private[ini] trait IniParseError { def msg: String }
private[ini] case class LexerError(msg: String) extends IniParseError
private[ini] case class ParserError(msg: String) extends IniParseError


private[ini] object IniLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[\t\r]+".r

  def lbrace   = "\\[".r      ^^ { _ => LBRACE }
  def rbrace   = "\\]".r      ^^ { _ => RBRACE }
  def dquote   = "\"".r       ^^ { _ => DQUOTE }
  def equals   = " *= *".r    ^^ { _ => EQUALS }
  def space    = " +".r       ^^ { _ => SPACE }
  def newline  = "\\n+".r     ^^ { _ => NEWLINE }
  def letter   = "[a-zA-Z]".r ^^ { s => LETTER(s) }
  def number   = "[0-9]".r    ^^ { s => NUMBER(s) }
  def any      = ".".r        ^^ { s => ANY(s) }

  def tokens: Parser[List[Token]] =
    phrase(rep1(lbrace | rbrace | dquote | equals | space | newline | letter | number | any)) ^^ { rawTokens =>
      rawTokens
    }

  def apply(code: String): Either[LexerError, List[Token]] =
    parse(tokens, code) match {
      case NoSuccess(msg, next)  => Left(LexerError(msg))
      case Success(result, next) => Right(result)
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

  def letter: Parser[LETTER] =
    accept("letter", { case l @ LETTER(_) => l })

  def number: Parser[NUMBER] =
    accept("number", { case n @ NUMBER(_) => n })

  def any: Parser[ANY] =
    accept("any", { case a @ ANY(_) => a })

  def word: Parser[Word] =
    rep1(letter) ^^ {
      case letters => Word(letters.mkString(""))
    }

  def quoted: Parser[Quoted] =
    (DQUOTE ~ rep1(letter | number) ~ DQUOTE) ^^ {
      case _ ~ chars ~ _ => Quoted(chars.mkString(""))
    }

  def anything: Parser[Word] =
    rep1(letter | number | any) ^^ { case chars => Word(chars.mkString("")) }

  def key: Parser[Key] =
    word ^^ { case Word(s) => Key(s) }

  def value: Parser[Value] =
    rep1(anything | SPACE | DQUOTE | RBRACE | LBRACE | EQUALS) ^^ { case chars => Value(chars.foldLeft("") { (memo, ch) =>
      ch match {
        case SPACE  => memo
        case EQUALS => memo.trim + "="
        case DQUOTE => memo.trim + "\""
        case LBRACE => memo.trim + "["
        case RBRACE => memo.trim + "]"
        case _      => memo + s"$ch "
      }
    }.trim)}

  def document: Parser[List[Section]] =
    phrase(rep1(section)) ^^ { case sections => sections }

  def section: Parser[Section] =
    (sectionHeader ~ rep1(keyValuePair)) ^^ {
      case h ~ ps => Section(h, ps)
    }

  def sectionHeader: Parser[SectionHeader] =
    (LBRACE ~ word ~ opt(SPACE ~ quoted) ~ RBRACE ~ NEWLINE) ^^ {
      case _ ~ Word(s1) ~ Some(_ ~ Quoted(s2)) ~ _ ~ _ => SectionHeader(s"$s1.$s2")
      case _ ~ Word(s) ~ None ~ _ ~ _                  => SectionHeader(s)
    }

  def keyValuePair: Parser[KeyValuePair] =
    (opt(SPACE) ~ key ~ EQUALS ~ value ~ NEWLINE) ^^ {
      case _ ~ (k @ Key(_)) ~ _ ~ (v @ Value(_)) ~ _ => KeyValuePair(k, v)
    }

  def apply(tokens: Seq[Token]): Either[ParserError, List[Section]] = {
    val reader = new IniTokenReader(tokens)
    document(reader) match {
      case NoSuccess(msg, next)  => Left(ParserError(msg))
      case Success(result, next) => Right(result)
    }
  }
}


object IniProcessor {
  def apply(code: String): Either[IniParseError, List[Section]] = {
    for {
      tokens <- IniLexer(code).right
      ast <- IniParser(tokens).right
    } yield ast
  }
}
