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
        case v @ _ => throw new IllegalStateException("Expecting object, found: " + v)
    }
    
    /**
     * Parses the input as a JSON object.  
     */
    def parseObject(input:Reader):JsonObject = parse(input) match {
        case obj @ JsonObject() => obj
        case v @ _ => throw new IllegalStateException("Expecting object, found: " + v)
    }
    
    /**
     * Parses the input as a JSON object.  
     */
    def parseObject(input:InputStream):JsonObject = parse(input) match {
        case obj @ JsonObject() => obj
        case v @ _ => throw new IllegalStateException("Expecting object, found: " + v)
    }
}
