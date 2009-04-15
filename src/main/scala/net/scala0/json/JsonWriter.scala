/**
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed
 * with this work for additional information regarding copyright
 * ownership.  The ASF licenses this file to you under the Apache
 * License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License.  You may obtain a copy of
 * the License at
 *    
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
 * implied.  See the License for the specific language governing
 * permissions and limitations under the License.
 */
package net.scala0.json

import java.io.{StringWriter,Writer}
import java.lang.{Iterable => JIterable}

class JsonWriter(out: Writer) {
    private var comma = false
    
    def this() = this(new StringWriter)
    
    override def toString: String = out.toString
    
    def value(v: JsonValue): JsonWriter = v match {
        case v @ JsonObject() => value(v)
        case v @ JsonArray() => value(v)
        case JsonString(v) => value(v)
        case JsonNumber(v) => value(v)
        case JsonBoolean(v) => value(v)
        case JsonNull => nullValue
        case null => nullValue
    }
    
    def value(obj: JsonObject): JsonWriter = {
        startObject
        for (bin <- obj) binding(bin)
        endObject
    }
    
    def value(array: JsonArray): JsonWriter = {
        startArray
        for (elem <- array) value(elem)
        endArray
    }
    
    def value(v: JsonString): JsonWriter = value(v.value)
    def value(v: JsonNumber): JsonWriter = value(v.value)
    def value(v: JsonBoolean): JsonWriter = value(v.value)
    
    def mapValue[V](v: V)(f: V=>JsonValue): JsonWriter = {
        if (v == null) nullValue else value(f(v))
    }
    
    def mapValue[V](opt: Option[V])(f: V=>JsonValue): JsonWriter = opt match {
        case None => nullValue
        case Some(v) => value(f(v))
    }
    
    def writeValue[V](opt: Option[V])(f: V=>Unit): JsonWriter = opt match {
        case None => nullValue
        case Some(v) => 
            f(v)
            this
    }
    
    def value(v: String): JsonWriter = {
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
    
    def value(v: Number): JsonWriter = print(v.toString)
    def value(v: Boolean): JsonWriter = print(v.toString)
    def nullValue: JsonWriter = print("null")
    def binding(bin: JsonBinding): JsonWriter = binding(bin.key, bin.value)
    
    def binding(k: String, v: JsonValue): JsonWriter = {
        key(k)
        value(v)
    }
    
    def key(k: String): JsonWriter = {
        value(k)
        out.write(":")
        comma = false
        this
    }
    
    def startObject: JsonWriter = {
        if (comma) out.write(',')
        out.write('{')
        comma = false
        this
    }
    
    def endObject: JsonWriter = {
        out.write('}')
        comma = true
        this
    }
    
    def writeObject[T](obj: T)(f: T=>Unit): JsonWriter = {
        if (obj == null) {
            nullValue
        }
        else {
            startObject
            f(obj)
            endObject
        }
    }
    
    def startArray: JsonWriter = {
        if (comma) out.write(',')
        out.write('[')
        comma = false
        this
    }
    
    def endArray: JsonWriter = {
        out.write(']')
        comma = true
        this
    }
    
    def writeArray[T](array: Iterable[T])(f: T=>Unit): JsonWriter = {
        if (array == null) {
            nullValue
        }
        else {
            startArray
            for (elem <- array) f(elem)
            endArray
        }
    }

    def writeArray[T](array: JIterable[T])(f: T=>Unit): JsonWriter = {
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

    def mapArray[T](values: Collection[T])(f: T=>JsonValue): JsonWriter = {
        if (values == null || values.size == 0) {
            nullValue
        }
        else {
            startArray
            for (v <- values) value(f(v))
            endArray
        }
    }
    
    def print(str: String): JsonWriter = {
        if (comma) out.write(',')
        out.write(str)
        comma = true
        this
    }
}
