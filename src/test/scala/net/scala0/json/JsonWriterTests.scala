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

class JsonWriterTests extends FunSuite {
    test("SimpleValues") {
        assert("null" === JsonNull.toString)
        assert("true" === JsonBoolean(true).toString)
        assert("false" === JsonBoolean(false).toString)
        assert("42" === JsonNumber(42).toString)
        assert("3.14159" === JsonNumber(3.14159).toString)
        assert("\"hello\\nworld\"" === JsonString("hello\nworld").toString)
    }
    
    test("Arrays") {
        var array = JsonArray()
        assert("[]" === array.toString)

        array += JsonNumber(1)
        assert("[1]" === array.toString)

        array += JsonNumber(2)
        assert("[1,2]" === array.toString)

        val subarray = JsonArray()
        subarray += JsonString("pony")
        array += subarray
        assert("[1,2,[\"pony\"]]" === array.toString)
    }
    
    test("Objects") {
        var obj = JsonObject()
        assert("{}" === obj.toString)
        
        obj("firstName") = JsonString("Jeremy")
        assert("{\"firstName\":\"Jeremy\"}" === obj.toString)
        
        obj("lastName") = JsonString("Cloud")
        assert("{\"firstName\":\"Jeremy\",\"lastName\":\"Cloud\"}" === obj.toString)
        
        val numbers = JsonArray()
        numbers += JsonNumber(42)
        numbers += JsonNumber(64)
        numbers += JsonNumber(99)
        obj("numbers") = numbers
        assert("{\"firstName\":\"Jeremy\",\"lastName\":\"Cloud\",\"numbers\":[42,64,99]}" === obj.toString)
    }
}