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

import scala.collection.mutable.ArrayBuffer

/**
 * JsReader builds JsValues using a JsTokenizer.  Typically,
 * you wouldn't use this class directly, you would just use the 
 * JSON class.
 */
class JsReader(tkr: JsTokenizer) {
    /**
     * Reads the next option value, returning Some(JsValue) if found,
     * or None if the end of the stream is reached
     */
    def readValue: Option[JsValue] = {
        tkr.next match {
            case None => None
            case Some(token) => Some(parseValue(token))
        }
    }
    
    /**
     * Parses the next value.  Throws JsException if the end
     * of the stream is reached.
     */
    def parseValue: JsValue = parseValue(nextToken)
    
    private def parseValue(token: JsToken): JsValue = {
        token.ttype match {
            case '"' => JsString(token.text)
            case '0' => parseNumber(token.text)
            case 'a' => 
                token.text match {
                    case "true" => JsBoolean(true)
                    case "false" => JsBoolean(false)
                    case "null" => JsNull
                    case _ => throw new JsException("Unexpected token: `" + token.text + "`")
                }
            case '{' => parseObject
            case '[' => parseArray
            case _ => throw new JsException("Unexpected token: `" + token.text + "`")
        }
    }

    private def parseNumber(text: String): JsValue = {
        if (text.indexOf('.') >= 0 || text.indexOf('e') >= 0 || text.indexOf('E') >= 0) {
            JsNumber(text.toDouble)
        }
        else {
            val longVal = text.toLong
            val intVal = text.toInt
            if (intVal == longVal) {
                JsNumber(intVal)
            }
            else {
                JsNumber(longVal)
            }
        }
    }
    
    private def parseObject: JsValue = {
        val buf = new JsObjectBuffer
        
        if (tkr.peek != '}') {
            buf += parseBinding
            
            while (tkr.peek == ',') {
                tkr.next // skip comma
                buf += parseBinding
            }
        }
        
        matchToken('}')
        // wrap with immutable JsObject
        JsObject(buf)
    }
    
    private def parseBinding: (String,JsValue) = {
        val key = nextToken match {
            case JsToken(ttype, text) =>
                ttype match {
                    case '"' => text
                    case 'a' => text
                    case _ => throw new JsException("Expecting string or identifier, found `" + text + "`")
                }
        }
        matchToken(':')
        (key, parseValue)
    }
    
    private def parseArray: JsValue = {
        val buf = new ArrayBuffer[JsValue]
        
        if (tkr.peek != ']') {
            buf += parseValue
            
            while (tkr.peek == ',') {
                tkr.next // skip comma
                buf += parseValue
            }
        }
        
        matchToken(']')
        JsArray(buf)
    }
    
    private def nextToken: JsToken = {
        tkr.next match {
            case None => throw new JsException("Unexpected end of input")
            case Some(token) => token
        }
    }
    
    private def matchToken(expectedType: Char): JsToken = {
        val token = nextToken
        if (token.ttype != expectedType) {
            throw new JsException("Unexpected token: `" + token.text + "`")
        }
        else {
            token
        }
    }
}
