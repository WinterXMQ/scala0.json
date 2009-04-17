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

class JsTokenizerTests extends FunSuite {
    test("EmptyString") {
        assert(None, new JsTokenizer("").next)
    }

    test("Numbers") {
        val tkr = new JsTokenizer("0 -10 3.14 1323.00 9e3 -9e-3 3.4e+50")
        assert(Some(JsToken('0', "0")) === tkr.next)
        assert(Some(JsToken('0', "-10")) === tkr.next)
        assert(Some(JsToken('0', "3.14")) === tkr.next)
        assert(Some(JsToken('0', "1323.00")) === tkr.next)
        assert(Some(JsToken('0', "9e3")) === tkr.next)
        assert(Some(JsToken('0', "-9e-3")) === tkr.next)
        assert(Some(JsToken('0', "3.4e+50")) === tkr.next)
    }
    
    test("Identifiers") {
        val tkr = new JsTokenizer(" true\nfalse\rnull _foo ")
        assert(Some(JsToken('a', "true")) === tkr.next)
        assert(Some(JsToken('a', "false")) === tkr.next)
        assert(Some(JsToken('a', "null")) === tkr.next)
        assert(Some(JsToken('a', "_foo")) === tkr.next)
    }
    
    test("Strings") {
        val tkr = new JsTokenizer("\"\" '' \"hello world!\" \"one\\n\\\"two\\\"\" '\\u0020\u0aBc'")
        assert(Some(JsToken('"', "")) === tkr.next)
        assert(Some(JsToken('"', "")) === tkr.next)
        assert(Some(JsToken('"', "hello world!")) === tkr.next)
        assert(Some(JsToken('"', "one\n\"two\"")) === tkr.next)
        assert(Some(JsToken('"', "\u0020\u0aBc")) === tkr.next)
    }
    
    test("Syntax") {
        val tkr = new JsTokenizer("{}[]:,")
        assert(Some(JsToken('{', "{")) === tkr.next)
        assert(Some(JsToken('}', "}")) === tkr.next)
        assert(Some(JsToken('[', "[")) === tkr.next)
        assert(Some(JsToken(']', "]")) === tkr.next)
        assert(Some(JsToken(':', ":")) === tkr.next)
        assert(Some(JsToken(',', ",")) === tkr.next)
    }
}
