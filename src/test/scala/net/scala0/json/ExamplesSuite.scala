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

class ExamplesSuite extends FunSuite {
    import JSON._
    
    test("glossary example") {
        val json = JSON.parse("""
            {
                "glossary": {
                    "title": "example glossary",
            		"GlossDiv": {
                        "title": "S",
            			"GlossList": {
                            "GlossEntry": {
                                "ID": "SGML",
            					"SortAs": "SGML",
            					"GlossTerm": "Standard Generalized Markup Language",
            					"Acronym": "SGML",
            					"Abbrev": "ISO 8879:1986",
            					"GlossDef": {
                                    "para": "A meta-markup language, used to create markup languages such as DocBook.",
            						"GlossSeeAlso": ["GML", "XML"]
                                },
            					"GlossSee": "markup"
                            }
                        }
                    }
                }
            }
        """)
        val expected = JsObject(
            "glossary" -> JsObject(
                "title" -> "example glossary", 
                "GlossDiv" -> JsObject(
                    "title" -> "S",
                    "GlossList" -> JsObject(
                        "GlossEntry"-> JsObject(
                            "ID" -> "SGML",
        					"SortAs" -> "SGML",
        					"GlossTerm" -> "Standard Generalized Markup Language",
        					"Acronym" -> "SGML",
        					"Abbrev" -> "ISO 8879:1986",
        					"GlossDef" -> JsObject(
                                "para" -> JsString("A meta-markup language, used to create markup languages such as DocBook."),
        						"GlossSeeAlso" -> JsArray("GML", "XML")
                            ),
        					"GlossSee"-> "markup"
                        )
                    )
                )
            )
        )
        
        assert(json === expected)
    }
    
    test("menu") {
        val json = JSON.parse("""
            {"menu": {
              "id": "file",
              "value": "File",
              "popup": {
                "menuitem": [
                  {"value": "New", "onclick": "CreateNewDoc()"},
                  {"value": "Open", "onclick": "OpenDoc()"},
                  {"value": "Close", "onclick": "CloseDoc()"}
                ]
              }
            }}
        """)
        val expected = JsObject(
            "menu" -> Map(
                "id" -> "file",
                "value" -> "File",
                "popup" -> Map(
                    "menuitem" -> List(
                        Map("value" -> "New", "onclick" -> "CreateNewDoc()"),
                        Map("value" -> "Open", "onclick" -> "OpenDoc()"),
                        Map("value" -> "Close", "onclick" -> "CloseDoc()")
                    )
                )
            )   
        )
        
        assert(json === expected)        
    }
}
