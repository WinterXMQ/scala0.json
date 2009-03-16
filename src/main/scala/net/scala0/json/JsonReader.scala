package net.scala0.json

/**
 * JsonReader builds JsonValues using a JsonTokenizer.  Typically,
 * you wouldn't use this class directly, you would just use the 
 * JsonParser class.
 */
class JsonReader(tkr:JsonTokenizer) {
    /**
     * Reads the next option value, returning Some(JsonValue) if found,
     * or None if the end of the stream is reached
     */
    def readValue:Option[JsonValue] = {
        tkr.next match {
            case None => None
            case Some(token) => Some(parseValue(token))
        }
    }
    
    /**
     * Parses the next value.  Throws JsonException if the end
     * of the stream is reached.
     */
    def parseValue:JsonValue = parseValue(nextToken)
    
    private def parseValue(token:JsonToken):JsonValue = {
        token.ttype match {
            case '"' => JsonString(token.text)
            case '0' => parseNumber(token.text)
            case 'a' => 
                token.text match {
                    case "true" => JsonBoolean(true)
                    case "false" => JsonBoolean(false)
                    case "null" => JsonNull
                    case _ => throw new JsonException("Unexpected token: `" + token.text + "`")
                }
            case '{' => parseObject
            case '[' => parseArray
            case _ => throw new JsonException("Unexpected token: `" + token.text + "`")
        }
    }

    private def parseNumber(text:String):JsonValue = {
        if (text.indexOf('.') >= 0 || text.indexOf('e') >= 0 || text.indexOf('E') >= 0) {
            JsonNumber(text.toDouble)
        }
        else {
            val longVal = text.toLong
            val intVal = text.toInt
            if (intVal == longVal) {
                JsonNumber(intVal)
            }
            else {
                JsonNumber(longVal)
            }
        }
    }
    
    private def parseObject:JsonValue = {
        val obj = new JsonObject()
        
        if (tkr.peek != '}') {
            obj += parseBinding
            
            while (tkr.peek == ',') {
                tkr.next // skip comma
                obj += parseBinding
            }
        }
        
        matchToken('}')
        obj
    }
    
    private def parseBinding:JsonBinding = {
        val key = nextToken match {
            case JsonToken(ttype, text) =>
                ttype match {
                    case '"' => text
                    case 'a' => text
                    case _ => throw new JsonException("Expecting string or identifier, found `" + text + "`")
                }
        }
        matchToken(':')
        val value = parseValue
        new JsonBinding(key, value)
    }
    
    private def parseArray:JsonValue = {
        val array = new JsonArray()
        
        if (tkr.peek != ']') {
            array += parseValue
            
            while (tkr.peek == ',') {
                tkr.next // skip comma
                array += parseValue
            }
        }
        
        matchToken(']')
        array
    }
    
    private def nextToken:JsonToken = {
        tkr.next match {
            case None => throw new JsonException("Unexpected end of input")
            case Some(token) => token
        }
    }
    
    private def matchToken(expectedType:char):JsonToken = {
        val token = nextToken
        if (token.ttype != expectedType) {
            throw new JsonException("Unexpected token: `" + token.text + "`")
        }
        else {
            token
        }
    }
}
