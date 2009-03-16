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

import junit.framework._
import junit.framework.Assert._

class JsonParserTests extends TestCase {
    private def testValue(value:JsonValue) = {
        assertEquals(value, JsonParser.parse(value.toString))
    }
        
    def testSimpleValues() = {
        testValue(JsonNull)
        testValue(JsonBoolean(true))
        testValue(JsonBoolean(false))
        testValue(JsonNumber(42))
        testValue(JsonNumber(3.14))
        testValue(JsonString("hello"))
    }

    def testArrays() = {
        testValue(JsonArray())
        testValue(JsonArray().add(JsonNumber(1)))
        testValue(JsonArray().add(JsonNumber(1)).add(JsonNumber(2)).add(JsonNumber(3)))
        testValue(JsonArray().add(JsonNumber(1)).add(JsonArray().add(JsonBoolean(true)).add(JsonString("hello"))))
    }
    
    def testObjects() = {
        testValue(JsonObject())
        testValue(JsonObject().put("first", JsonString("Jeremy")))
        testValue(JsonObject().put("first", JsonString("Jeremy")).put("last", JsonString("Cloud")))
        testValue(JsonObject().put("a", JsonObject().put("b", JsonBoolean(false))).put("c", JsonArray().add(JsonString("green"))))
    }
}
