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

import java.io._

/**
 * The JsonParser object provides entry point methods for parsing json.
 */
object JsonParser {
    /**
     * Parses the input as any JSON value.
     */
    def parse(source:String):JsonValue = {
        new JsonReader(new JsonTokenizer(source)).parseValue
    }
    
    /**
     * Parses the input as any JSON value.
     */
    def parse(input:Reader):JsonValue = {
        new JsonReader(new JsonTokenizer(input)).parseValue
    }
    
    /**
     * Parses the input as any JSON value.
     */
    def parse(input:InputStream):JsonValue = {
        new JsonReader(new JsonTokenizer(new InputStreamReader(input))).parseValue
    }

    /**
     * Parses the input as a JSON object.  
     */
    def parseObject(source:String):JsonObject = parse(source) match {
        case obj @ JsonObject() => obj
        case v @ _ => throw new JsonException("Expecting object, found: " + v)
    }
    
    /**
     * Parses the input as a JSON object.  
     */
    def parseObject(input:Reader):JsonObject = parse(input) match {
        case obj @ JsonObject() => obj
        case v @ _ => throw new JsonException("Expecting object, found: " + v)
    }
    
    /**
     * Parses the input as a JSON object.  
     */
    def parseObject(input:InputStream):JsonObject = parse(input) match {
        case obj @ JsonObject() => obj
        case v @ _ => throw new JsonException("Expecting object, found: " + v)
    }
}
