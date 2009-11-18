/*
    Copyright 2009 Jeremy Cloud

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/
package net.scala0.json

import java.io.{Reader, StringReader}

/**
 * Tokenizes a Reader stream into JsToken objects
 */
class JsTokenizer(input: Reader) {
    import java.lang.Character._
    
    private var pushedChar: Char = 0
    private var pushedToken: Option[JsToken] = null
    
    def this(source: String) {
        this(new StringReader(source))
    }
    
    /**
     * Looks ahead at the type of the next token.
     * Possible values are: '{' '}', '[', ']', ':', ',', '"', '0', 0
     */
    def peek: Char = {
        if (pushedToken == null) {
            pushedToken = next
        }
        
        pushedToken match {
            case None => 0
            case Some(JsToken(ttype, _)) => ttype
        }
    }
    
    def next: Option[JsToken] = {
        if (pushedToken != null) {
            val res = pushedToken
            pushedToken = null
            return res
        }
        
        nextChar match {
            case '{' => Some(JsToken('{', "{"))
            case '}' => Some(JsToken('}', "}"))
            case '[' => Some(JsToken('[', "["))
            case ']' => Some(JsToken(']', "]"))
            case ':' => Some(JsToken(':', ":"))
            case ',' => Some(JsToken(',', ","))
            case '"' => Some(JsToken('"', parseDoubleQuotedString()))
            case '\'' => Some(JsToken('"', parseSingleQuotedString()))
            case '-' => Some(JsToken('0', parseNumber('-')))
            case 0 => None
            case c: Char =>
                if (isWhitespace(c))
                    next
                else if (Character.isDigit(c))
                    Some(JsToken('0', parseNumber(c)))
                else if (isJavaIdentifierPart(c))
                    Some(JsToken('a', parseIdentifier(c)))
                else
                    throw new JsException("Unexpected character '" + c + "'")
        }
    }
    
    private def nextChar: Char = {
        if (pushedChar != 0) {
            val res = pushedChar
            pushedChar = 0
            res
        }
        else {
            val c = input.read
            if (c < 0) 0 else c.toChar
        }
    }
    
    private def peekChar: Char = {
        if (pushedChar == 0) {
            pushedChar = nextChar
        }
        
        pushedChar
    }
    
    private def parseDoubleQuotedString(): String = {
        val sb = new StringBuilder()
        var esc = false

        while (true) {
            if (esc) {
                nextChar match {
                    case 0 => throw new JsException("Unclosed string literal")
                    case 'b' => sb.append('\b')
                    case 'f' => sb.append('\f')
                    case 'n' => sb.append('\n')
                    case 'r' => sb.append('\r')
                    case 't' => sb.append('\t')
                    case 'u' => sb.append(parseHexCode())
                    case c: Char => sb.append(c.toChar)
                }
                esc = false
            }
            else {
                nextChar match {
                    case 0 => throw new JsException("Unclosed string literal")
                    case '\\' => esc = true
                    case '"' => return sb.toString
                    case c: Char => sb.append(c)
                }
            }
        }

        sb.toString
    }
    
    
    private def parseSingleQuotedString(): String = {
        val sb = new StringBuilder()        
        var esc = false

        while (true) {
            if (esc) {
                nextChar match {
                    case 0 => throw new JsException("Unclosed string literal")
                    case 'b' => sb.append('\b')
                    case 'f' => sb.append('\f')
                    case 'n' => sb.append('\n')
                    case 'r' => sb.append('\r')
                    case 't' => sb.append('\t')
                    case 'u' => sb.append(parseHexCode())
                    case c: Char => sb.append(c.toChar)
                }
                esc = false
            }
            else {
                nextChar match {
                    case 0 => throw new JsException("Unclosed string literal")
                    case '\\' => esc = true
                    case '\'' => return sb.toString
                    case c: Char => sb.append(c)
                }
            }
        }

        sb.toString
    }
    
    
    private def parseHexCode(): Char = {
        return ((parseHexChar << 12) | (parseHexChar << 8) | (parseHexChar << 4) | parseHexChar).toChar
    }
    
    
    private def parseHexChar: Int = {
        val c = nextChar
        
        if (c == 0) 
            throw new JsException("Expecting hex character, but reached end of stream")
        else if (c >= '0' && c <= '9')
            c - '0'
        else if (c >= 'a' && c <= 'f')
            c - 'a' + 10
        else if (c >= 'A' && c <= 'F')
            c - 'A' + 10
        else
            throw new JsException("Expecting hex character, found '" + c + "'")
    }
    
    
    private def parseNumber(start: Char): String = {
        val sb = new StringBuilder()

        sb.append(start)
        
        while (isDigit(peekChar))
            sb.append(nextChar)
            
        if (peekChar == '.') {
            sb.append(nextChar)

            while (isDigit(peekChar))
                sb.append(nextChar)
        }
        
        if (peekChar == 'e' || peekChar == 'E') {
            sb.append(nextChar)

            if (peekChar == '-' || peekChar == '+') {
                sb.append(nextChar)
            }

            while (isDigit(peekChar))
                sb.append(nextChar)
        }

        sb.toString
    }
    
    private def parseIdentifier(start: Char): String = {
        val sb = new StringBuilder()

        sb.append(start)
        
        while (peekChar > 0 && isJavaIdentifierPart(peekChar)) {
            sb.append(nextChar.toChar)
        }

        sb.toString
    }
}

case class JsToken(ttype: Char, text: String)
