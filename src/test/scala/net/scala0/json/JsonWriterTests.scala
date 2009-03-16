package net.scala0.json

import junit.framework._
import junit.framework.Assert._

class JsonWriterTests extends TestCase {
    def testSimpleValues = {
        assertEquals("null", JsonNull.toString)
        assertEquals("true", JsonBoolean(true).toString)
        assertEquals("false", JsonBoolean(false).toString)
        assertEquals("42", JsonNumber(42).toString)
        assertEquals("3.14159", JsonNumber(3.14159).toString)
        assertEquals("\"hello\\nworld\"", JsonString("hello\nworld").toString)
    }
    
    def testArrays = {
        var array = JsonArray()
        assertEquals("[]", array.toString)

        array += JsonNumber(1)
        assertEquals("[1]", array.toString)

        array += JsonNumber(2)
        assertEquals("[1,2]", array.toString)

        val subarray = JsonArray()
        subarray += JsonString("pony")
        array += subarray
        assertEquals("[1,2,[\"pony\"]]", array.toString)
    }
    
    def testObjects = {
        var obj = JsonObject()
        assertEquals("{}", obj.toString)
        
        obj("firstName") = JsonString("Jeremy")
        assertEquals("{\"firstName\":\"Jeremy\"}", obj.toString)
        
        obj("lastName") = JsonString("Cloud")
        assertEquals("{\"firstName\":\"Jeremy\",\"lastName\":\"Cloud\"}", obj.toString)
        
        val numbers = JsonArray()
        numbers += JsonNumber(42)
        numbers += JsonNumber(64)
        numbers += JsonNumber(99)
        obj("numbers") = numbers
        assertEquals("{\"firstName\":\"Jeremy\",\"lastName\":\"Cloud\",\"numbers\":[42,64,99]}", obj.toString)
    }
}