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

class JsonTokenizerTests extends TestCase {
    def testEmptyString() = {
        assertEquals(None, new JsonTokenizer("").next)
    }

    def testNumbers() = {
        val tkr = new JsonTokenizer("0 -10 3.14 1323.00 9e3 -9e-3 3.4e+50")
        assertEquals(Some(JsonToken('0', "0")), tkr.next)
        assertEquals(Some(JsonToken('0', "-10")), tkr.next)
        assertEquals(Some(JsonToken('0', "3.14")), tkr.next)
        assertEquals(Some(JsonToken('0', "1323.00")), tkr.next)
        assertEquals(Some(JsonToken('0', "9e3")), tkr.next)
        assertEquals(Some(JsonToken('0', "-9e-3")), tkr.next)
        assertEquals(Some(JsonToken('0', "3.4e+50")), tkr.next)
    }
    
    def testIdentifiers() = {
        val tkr = new JsonTokenizer(" true\nfalse\rnull _foo ")
        assertEquals(Some(JsonToken('a', "true")), tkr.next)
        assertEquals(Some(JsonToken('a', "false")), tkr.next)
        assertEquals(Some(JsonToken('a', "null")), tkr.next)
        assertEquals(Some(JsonToken('a', "_foo")), tkr.next)
    }
    
    def testStrings() = {
        val tkr = new JsonTokenizer("\"\" '' \"hello world!\" \"one\\n\\\"two\\\"\" '\\u0020\u0aBc'")
        assertEquals(Some(JsonToken('"', "")), tkr.next)
        assertEquals(Some(JsonToken('"', "")), tkr.next)
        assertEquals(Some(JsonToken('"', "hello world!")), tkr.next)
        assertEquals(Some(JsonToken('"', "one\n\"two\"")), tkr.next)
        assertEquals(Some(JsonToken('"', "\u0020\u0aBc")), tkr.next)
    }
    
    def testSyntax() = {
        val tkr = new JsonTokenizer("{}[]:,")
        assertEquals(Some(JsonToken('{', "{")), tkr.next)
        assertEquals(Some(JsonToken('}', "}")), tkr.next)
        assertEquals(Some(JsonToken('[', "[")), tkr.next)
        assertEquals(Some(JsonToken(']', "]")), tkr.next)
        assertEquals(Some(JsonToken(':', ":")), tkr.next)
        assertEquals(Some(JsonToken(',', ",")), tkr.next)
    }
}
