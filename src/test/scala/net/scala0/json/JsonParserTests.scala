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