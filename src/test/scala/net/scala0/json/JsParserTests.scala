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

import org.scalatest.FunSuite

class JsParserTests extends FunSuite {
    import JSON._
    
    private def testValue(value: JsValue) = {
        assert(value === JSON.parse(value.toString))
    }
        
    test("simple values") {
        testValue(JsNull)
        testValue(JsBoolean(true))
        testValue(JsBoolean(false))
        testValue(JsNumber(42))
        testValue(JsNumber(3.14))
        testValue(JsString("hello"))
    }

    test("arrays") {
        testValue(JsArray())
        testValue(JsArray(1))
        testValue(JsArray(1, 2, 3))
        testValue(JsArray(1, JsArray(true, "hello")))
    }
    
    test("objects") {
        testValue(JsObject())
        testValue(JsObject("first" -> "Jeremy"))
        testValue(JsObject("first" -> "Jeremy", "last" -> "Cloud"))
        testValue(JsObject("a" -> JsObject("b" -> false), "c" -> JsArray("green")))
    }
}
