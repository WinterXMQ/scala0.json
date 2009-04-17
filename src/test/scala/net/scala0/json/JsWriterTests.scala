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

class JsWriterTests extends FunSuite {
    import JSON._
    
    test("SimpleValues") {
        assert("null" === JsNull.toString)
        assert("true" === JsBoolean(true).toString)
        assert("false" === JsBoolean(false).toString)
        assert("42" === JsNumber(42).toString)
        assert("3.14159" === JsNumber(3.14159).toString)
        assert("\"hello\\nworld\"" === JsString("hello\nworld").toString)
    }
    
    test("Arrays") {
        assert("[]" === JsArray().toString)
        assert("[1]" === JsArray(1).toString)
        assert("[1,2]" === JsArray(1, 2).toString)
    }
    
    test("Objects") {
        val obj = new JsObjectBuffer
        assert("{}" === obj.toString)
        
        obj("firstName") = "Jeremy"
        assert("{\"firstName\":\"Jeremy\"}" === obj.toString)
        
        obj("lastName") = "Cloud"
        assert("{\"firstName\":\"Jeremy\",\"lastName\":\"Cloud\"}" === obj.toString)
        
        obj("numbers") = JsArray(42, 64, 99)
        assert("{\"firstName\":\"Jeremy\",\"lastName\":\"Cloud\",\"numbers\":[42,64,99]}" === obj.toString)
    }
}