package net.scala0.json

import java.io._
import scala.collection.mutable.ArrayBuffer

/**
 * The root JsonValue class.
 */
trait JsonValue {
    override def toString:String = {
        val sp = new StringWriter()
        new JsonWriter(sp).value(this)
        sp.toString()
    }
}

/**
 * A JsonNumber wraps a Number, which might be a Double or a Long.
 */
case class JsonNumber(val value:Number) extends JsonValue
case class JsonString(val value:String) extends JsonValue
case class JsonBoolean(val value:Boolean) extends JsonValue
/**
 * JsonNull is used to represent the 'null' value in Json
 */
object JsonNull extends JsonValue

case class JsonArray extends ArrayBuffer[JsonValue] with JsonValue {
    def add(value:JsonValue):JsonArray = {
        this += value
        this
    }
    def addAll(values:Collection[JsonValue]):JsonArray = {
        this ++= values
        this
    }
}

case class JsonObject extends ArrayBuffer[JsonBinding] with JsonValue {
//    override def equals(any:Any):boolean = ArrayBuffer[JsonBinding].super.equals(any)
    
    def apply(key:String):Option[JsonValue] = find(_.key == key).map(_.value)
    
    def getString(key:String):String = {
        this(key) match {
            case None => throw new IllegalStateException("Key not found: `" + key + "` in " + this)
            case Some(v) => v match {
                case JsonString(str) => str
                case JsonNumber(num) => num.toString
                case JsonBoolean(bool) => bool.toString
                case JsonNull => throw new IllegalStateException("Expecting string, found null, for key `" + key + "` in " + this)
                case JsonArray() => throw new IllegalStateException("Expecting string, found array, for key `" + key + "` in " + this)
                case JsonObject() => throw new IllegalStateException("Expecting string, found object, for key `" + key + "` in " + this)
            }
        }
    }
    
    def getOptString(key:String):Option[String] = {
        this(key) match {
            case None => None
            case Some(v) => v match {
                case JsonString(str) => Some(str)
                case JsonNumber(num) => Some(num.toString)
                case JsonBoolean(bool) => Some(bool.toString)
                case JsonNull => None
                case JsonArray() => throw new IllegalStateException("Expecting string, found array, for key `" + key + "` in " + this)
                case JsonObject() => throw new IllegalStateException("Expecting string, found object, for key `" + key + "` in " + this)
            }
        }
    }
    
    def getNumber(key:String):Number = {
        this(key) match {
            case None => throw new IllegalStateException("Key not found: `" + key + "` in " + this)
            case Some(v) => v match {
                case JsonNumber(num) => num
                case JsonBoolean(bool) => if (bool) 1 else 0
                case JsonString(str) => str.toDouble
                case JsonNull => throw new IllegalStateException("Expecting number, found null, for key `" + key + "` in " + this)
                case JsonArray() => throw new IllegalStateException("Expecting number, found array, for key `" + key + "` in " + this)
                case JsonObject() => throw new IllegalStateException("Expecting number, found object, for key `" + key + "` in " + this)
            }
        }
    }
    
    def getOptNumber(key:String):Option[Number] = {
        this(key) match {
            case None => None
            case Some(v) => v match {
                case JsonNumber(num) => Some(num)
                case JsonBoolean(bool) => Some(if (bool) 1 else 0)
                case JsonString(str) => Some(str.toDouble)
                case JsonNull => None
                case JsonArray() => throw new IllegalStateException("Expecting number, found array, for key `" + key + "` in " + this)
                case JsonObject() => throw new IllegalStateException("Expecting number, found object, for key `" + key + "` in " + this)
            }
        }
    }
    
    def getBoolean(key:String):Boolean = {
        this(key) match {
            case None => throw new IllegalStateException("Key not found: `" + key + "` in " + this)
            case Some(v) => v match {
                case JsonBoolean(bool) => bool
                case JsonString(str) => throw new IllegalStateException("Expecting boolean, found string, for key `" + key + "` in " + this)
                case JsonNumber(num) => throw new IllegalStateException("Expecting boolean, found number, for key `" + key + "` in " + this)
                case JsonNull => throw new IllegalStateException("Expecting boolean, found null, for key `" + key + "` in " + this)
                case JsonArray() => throw new IllegalStateException("Expecting boolean, found array, for key `" + key + "` in " + this)
                case JsonObject() => throw new IllegalStateException("Expecting boolean, found object, for key `" + key + "` in " + this)
            }
        }
    }
    
    def getOptBoolean(key:String):Option[Boolean] = {
        this(key) match {
            case None => None
            case Some(v) => v match {
                case JsonBoolean(bool) => Some(bool)
                case JsonNull => None
                case JsonString(str) => throw new IllegalStateException("Expecting boolean, found string, for key `" + key + "` in " + this)
                case JsonNumber(num) => throw new IllegalStateException("Expecting boolean, found number, for key `" + key + "` in " + this)
                case JsonArray() => throw new IllegalStateException("Expecting boolean, found array, for key `" + key + "` in " + this)
                case JsonObject() => throw new IllegalStateException("Expecting boolean, found object, for key `" + key + "` in " + this)
            }
        }
    }
    
    def getObject(key:String):JsonObject = {
        this(key) match {
            case None => throw new IllegalStateException("Key not found: `" + key + "` in " + this)
            case Some(v) => v match {
                case obj @ JsonObject() => obj
                case JsonBoolean(bool) => throw new IllegalStateException("Expecting object, found boolean, for key `" + key + "` in " + this)
                case JsonString(str) => throw new IllegalStateException("Expecting object, found string, for key `" + key + "` in " + this)
                case JsonNumber(num) => throw new IllegalStateException("Expecting object, found number, for key `" + key + "` in " + this)
                case JsonNull => throw new IllegalStateException("Expecting object, found null, for key `" + key + "` in " + this)
                case JsonArray() => throw new IllegalStateException("Expecting object, found array, for key `" + key + "` in " + this)
            }
        }
    }
    
    def getOptObject(key:String):Option[JsonObject] = {
        this(key) match {
            case None => None
            case Some(v) => v match {
                case obj @ JsonObject() => Some(obj)
                case JsonNull => None
                case JsonBoolean(bool) => throw new IllegalStateException("Expecting object, found boolean, for key `" + key + "` in " + this)
                case JsonString(str) => throw new IllegalStateException("Expecting object, found string, for key `" + key + "` in " + this)
                case JsonNumber(num) => throw new IllegalStateException("Expecting object, found number, for key `" + key + "` in " + this)
                case JsonArray() => throw new IllegalStateException("Expecting object, found array, for key `" + key + "` in " + this)
            }
        }
    }
    
    def getArray(key:String):JsonArray = {
        this(key) match {
            case None => throw new IllegalStateException("Key not found: `" + key + "` in " + this)
            case Some(v) => v match {
                case arr @ JsonArray() => arr
                case obj @ JsonObject() => throw new IllegalStateException("Expecting array, found object, for key `" + key + "` in " + this)
                case JsonBoolean(bool) => throw new IllegalStateException("Expecting array, found boolean, for key `" + key + "` in " + this)
                case JsonString(str) => throw new IllegalStateException("Expecting array, found string, for key `" + key + "` in " + this)
                case JsonNumber(num) => throw new IllegalStateException("Expecting array, found number, for key `" + key + "` in " + this)
                case JsonNull => throw new IllegalStateException("Expecting array, found null, for key `" + key + "` in " + this)
            }
        }
    }
    
    def getOptArray(key:String):Option[JsonArray] = {
        this(key) match {
            case None => None
            case Some(v) => v match {
                case arr @ JsonArray() => Some(arr)
                case JsonNull => None
                case obj @ JsonObject() => throw new IllegalStateException("Expecting array, found object, for key `" + key + "` in " + this)
                case JsonBoolean(bool) => throw new IllegalStateException("Expecting array, found boolean, for key `" + key + "` in " + this)
                case JsonString(str) => throw new IllegalStateException("Expecting array, found string, for key `" + key + "` in " + this)
                case JsonNumber(num) => throw new IllegalStateException("Expecting array, found number, for key `" + key + "` in " + this)
            }
        }
    }
    
    def update(key:String, value:JsonValue) = append(JsonBinding(key, value))
    def put(key:String, value:JsonValue):JsonObject = {
        append(JsonBinding(key, value))
        this
    }
    def addAll(values:Collection[JsonBinding]):JsonObject = {
        this ++= values
        this
    }
}

case class JsonBinding(val key:String, val value:JsonValue)
