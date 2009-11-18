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

import java.io.{StringWriter,Writer}
import java.lang.{Iterable => JIterable}

class JsWriter(out: Writer) {
    private var comma = false
    
    def this() = this(new StringWriter)
    
    override def toString: String = out.toString
    
    def value(v: JsValue): JsWriter = v match {
        case v: JsObject => value(v)
        case v: JsArray => value(v)
        case JsString(v) => value(v)
        case JsNumber(v) => value(v)
        case JsBoolean(v) => value(v)
        case JsNull => nullValue
        case null => nullValue
        case JsUndefined => throw new IllegalArgumentException("Cannot write value: undefined")
    }
    
    def value(obj: JsObject): JsWriter = {
        startObject
        obj.foreach { 
            case (k, v) => key(k).value(v)
        }
        endObject
    }
    
    def value(array: JsArray): JsWriter = {
        startArray
        array.foreach(e => value(e))
        endArray
    }
    
    def value(v: JsString): JsWriter = value(v.value)
    def value(v: JsNumber): JsWriter = value(v.value)
    def value(v: JsBoolean): JsWriter = value(v.value)
    
    def mapValue[V](v: V)(f: V=>JsValue): JsWriter = {
        if (v == null) nullValue else value(f(v))
    }
    
    def mapValue[V](opt: Option[V])(f: V=>JsValue): JsWriter = opt match {
        case None => nullValue
        case Some(v) => value(f(v))
    }
    
    def writeValue[V](opt: Option[V])(f: V=>Unit): JsWriter = opt match {
        case None => nullValue
        case Some(v) => 
            f(v)
            this
    }
    
    def value(v: String): JsWriter = {
        if (v == null) {
            nullValue
        }
        else {
            if (comma) out.write(',')
            out.write('"')
            for (i <- List.range(0, v.length)) {
                v.charAt(i) match {
                    case '"' => out.write("\\\"")
                    case '\\' => out.write("\\\\")
                    case '\b' => out.write("\\b")
                    case '\f' => out.write("\\f")
                    case '\n' => out.write("\\n")
                    case '\r' => out.write("\\r")
                    case '\t' => out.write("\\t")
                    case c: Char =>
                        if (c < ' ' || (c >= '\u0080' && c < '\u00a0') || (c >= '\u2000' && c < '\u2100')) {
                            val t = "000" + Integer.toHexString(c)
                            out.append("\\u" + t.substring(t.length() - 4))
                        } 
                        else {
                            out.append(c)
                        }
                }            
            }
            out.write('"')
            comma = true
            this
        }
    }
    
    def value(v: Number): JsWriter = print(v.toString)
    def value(v: Boolean): JsWriter = print(v.toString)
    def nullValue: JsWriter = print("null")
    
    def binding(k: String, v: JsValue): JsWriter = {
        key(k)
        value(v)
    }
    
    def key(k: String): JsWriter = {
        value(k)
        out.write(":")
        comma = false
        this
    }
    
    def startObject: JsWriter = {
        if (comma) out.write(',')
        out.write('{')
        comma = false
        this
    }
    
    def endObject: JsWriter = {
        out.write('}')
        comma = true
        this
    }
    
    def writeObject[T](obj: T)(f: T=>Unit): JsWriter = {
        if (obj == null) {
            nullValue
        }
        else {
            startObject
            f(obj)
            endObject
        }
    }
    
    def startArray: JsWriter = {
        if (comma) out.write(',')
        out.write('[')
        comma = false
        this
    }
    
    def endArray: JsWriter = {
        out.write(']')
        comma = true
        this
    }
    
    def writeArray[T](array: Iterable[T])(f: T=>Unit): JsWriter = {
        if (array == null) {
            nullValue
        }
        else {
            startArray
            for (elem <- array) f(elem)
            endArray
        }
    }

    def writeArray[T](array: JIterable[T])(f: T=>Unit): JsWriter = {
        if (array == null) {
            nullValue
        }
        else {
            startArray
            val iterator = array.iterator
            while (iterator.hasNext) f(iterator.next)
            endArray
        }
    }

    def mapArray[T](values: Collection[T])(f: T=>JsValue): JsWriter = {
        if (values == null || values.size == 0) {
            nullValue
        }
        else {
            startArray
            for (v <- values) value(f(v))
            endArray
        }
    }
    
    def print(str: String): JsWriter = {
        if (comma) out.write(',')
        out.write(str)
        comma = true
        this
    }
}
