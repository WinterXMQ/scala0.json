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

import java.io._
import scala.collection.Map

/**
 * The JSON object provides entry point methods for parsing json.
 */
object JSON {
    implicit def toJs(str: String) = JsString(str)
    implicit def toJs(num: Int) = JsNumber(num)
    implicit def toJs(num: Long) = JsNumber(num)
    implicit def toJs(num: Float) = JsNumber(num)
    implicit def toJs(num: Double) = JsNumber(num)
    implicit def toJs(num: Number) = JsNumber(num)
    implicit def toJs(bool: Boolean) = JsBoolean(bool)

    /**
     * Converts any value to a JsValue, is possible.  Throws
     * a JsException if not possible.
     */
    def toJs(any: Any): JsValue = any match {
        case null => JsNull
        case js: JsValue => js
        case str: String => JsString(str)
        case num: Number => JsNumber(num)
        case bool: Boolean => JsBoolean(bool)
        case map: Map[String,_] => JsObject(map)
        case seq: Seq[_] => JsArray(seq.map(toJs(_)))
        case a => JsString(a.toString)
    }

    /**
     * Parses the input as any JSON value.
     */
    def parse(source: String): JsValue = {
        new JsReader(new JsTokenizer(source)).parseValue
    }
    
    /**
     * Parses the input as any JSON value.
     */
    def parse(input: Reader): JsValue = {
        new JsReader(new JsTokenizer(input)).parseValue
    }
    
    /**
     * Parses the input as any JSON value.
     */
    def parse(input: InputStream): JsValue = {
        new JsReader(new JsTokenizer(new InputStreamReader(input))).parseValue
    }

    /**
     * Parses the input as a JSON object.  
     */
    def parseObject(source: String): JsObject = parse(source) match {
        case obj: JsObject => obj
        case v => throw new JsException("Expecting object, found: " + v)
    }
    
    /**
     * Parses the input as a JSON object.  
     */
    def parseObject(input: Reader): JsObject = parse(input) match {
        case obj: JsObject => obj
        case v => throw new JsException("Expecting object, found: " + v)
    }
    
    /**
     * Parses the input as a JSON object.  
     */
    def parseObject(input: InputStream): JsObject = parse(input) match {
        case obj: JsObject => obj
        case v => throw new JsException("Expecting object, found: " + v)
    }
}
