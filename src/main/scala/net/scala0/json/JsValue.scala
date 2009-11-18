/*
    Copyright 2009 Jeremy Cloud

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/
package net.scala0.json

import java.io._
import scala.collection.{Map => BaseMap}
import scala.collection.immutable.HashMap
import scala.collection.mutable.{ArrayBuffer, Buffer, LinkedHashMap}
import JSON.toJs

/**
 * The root JsValue class.
 */
sealed trait JsValue {
    override def toString: String = {
        val sp = new StringWriter
        new JsWriter(sp).value(this)
        sp.toString
    }
    
    /**
    * Gets a subvalue by key.  Only applies to JsObjects
     */
    def apply(key: String): JsValue = JsUndefined

    /**
     * Gets a subvalue by index.  Only applies to JsArray
     */
    def apply(index: Int): JsValue = JsUndefined

    /**
     * Gets the text of this value.  For JsString, it is the value of the string.  For everything else,
     * this returns the JSON encoding of this value.
     */
    def text: String = toString
    def toBoolean: Boolean = throw new JsException("Cannot convert " + this + " to a boolean")
    def toNumber: Number = throw new JsException("Cannot convert " + this + " to a number")
    def toInt: Int = toNumber.intValue
    def toLong: Long = toNumber.longValue
    def toFloat: Float = toNumber.floatValue
    def toDouble: Double = toNumber.doubleValue
    def asArray = this.asInstanceOf[JsArray]
    def asObject = this.asInstanceOf[JsObject]
}

/**
 * A JsNumber wraps a Number, which might be a Double or a Long.
 */
case class JsNumber(value: Number) extends JsValue {
    override def toString = value.toString
    override val toNumber = value
}

case class JsBoolean(value: Boolean) extends JsValue {
    override def toString = value.toString
    override val toBoolean = value
}

case class JsString(value: String) extends JsValue {
    override val text = value    
}

/**
 * JsNull is used to represent the 'null' value in json
 */
case object JsNull extends JsValue  {
    override def toString = "null"
}

/**
 * JsUndefined is used to indicate the absense of a value in a JsObject.
 */
case object JsUndefined extends JsValue  {
    override def toString = "undefined"
}

/**
 * An immutable representation of a json-array
 */
trait JsArray extends Seq[JsValue] with JsValue {
    /**
     * Two JsArrays are equal if they contain the same elements, even if
     * they are different classes.
     */
    override def equals(a: Any) = a match {
        case that: JsArray => equalsWith(that)(_ == _)
        case _ => false
    }
}

object JsArray {
    /**
     * An empty array.
     */
    val empty = apply(List[JsValue]())
    
    def apply(): JsArray = empty
    
    /**
     * Builds an immutable JsArray from a sequence of values.
     */
    def apply(head: Any, tail: Any*): JsArray = {
        val buf = new JsArrayBuffer
        buf += toJs(head)
        tail.foreach(buf += toJs(_))
        apply(buf)        
    }
        
    /**
     * Builds an immutable JsArray backed by the given sequence of values.
     */
    /*
    def apply(values: Seq[Any]): JsArray = {
        val buf = new JsArrayBuffer
        values.foreach(buf += toJs(_))
        apply(buf)
    }
    */
    
    /**
     * Builds an immutable JsArray backed by the given sequence of values.
     */
    def apply(values: Seq[JsValue]): JsArray = new JsArray {
        def length = values.size
        def elements = values.elements
        override def apply(index: Int) = values(index)
    }
    
    def unapplySeq(arr: JsArray): Option[Seq[JsValue]] = Some(arr)
}

trait JsMutableArray extends Buffer[JsValue] with RandomAccessSeq.Mutable[JsValue] with JsArray

/**
 * A mutable, ArrayBuffer-based JsArray
 */
class JsArrayBuffer extends ArrayBuffer[JsValue] with JsMutableArray {
    override def apply(index: Int): JsValue = super[ArrayBuffer].apply(index)
}

object JsMutableArray {
    /**
     * Builds a mutable JsArrayBuffer from a sequence of values.
     */
    def apply(head: Any, tail: Any*): JsMutableArray = {
        val buf = new JsArrayBuffer
        buf += toJs(head)
        tail.foreach(buf += toJs(_))
        buf
    }
    
    /**
     * Builds a mutable JsArrayBuffer from a sequence of values.
     */
    def apply(values: Seq[Any]): JsMutableArray = {
        val buf = new JsArrayBuffer
        values.foreach(buf += toJs(_))
        buf
    }
}

/**
 * An immutable implementation of a json-object (associative array)
 */
trait JsObject extends BaseMap[String,JsValue] with JsValue {
    /**
     * Gets the specified value, or JsUndefined if not present.
     */
    override def apply(key: String): JsValue = get(key).getOrElse(JsUndefined)
    
    /**
     * Gets the specified string value.  If the value is not a string or not
     * present, then this returns None.
     */
    def getString(key: String): Option[String] = apply(key) match {
        case JsString(str) => Some(str)
        case _ => None
    }

    /**
     * Gets the specified value as a boolean, if applicable.  If the value is 
     * stored as a boolean, or as one of the strings "true" or "false".  Returns
     * None if the value is not present or does not represent a boolean.
     */
    def getBoolean(key: String): Option[Boolean] = apply(key) match {
        case JsBoolean(b) => Some(b)
        case JsString("true") => Some(true)
        case JsString("false") => Some(false)
        case _ => None
    }

    /**
     * Gets the specified value as a Number, if applicable.  Returns
     * None if the value is not present or does not represent a number.
     */
    def getNumber(key: String): Option[Number] = apply(key) match {
        case JsNumber(n) => Some(n)
        case JsString(str) => 
            try {
                Some(if (str.contains('.')) str.toDouble else str.toLong)
            }
            catch {
                case e: NumberFormatException => None
            }
        case _ => None
    }

    /**
     * Gets the specified value as a Long, if applicable.  Returns
     * None if the value is not present or does not represent a Long.
     */
    def getLong(key: String): Option[Long] = apply(key) match {
        case JsNumber(n) => Some(n.longValue)
        case JsString(str) => 
            try {
                Some(str.toLong)
            }
            catch {
                case e: NumberFormatException => None
            }
        case _ => None
    }

    /**
     * Gets the specified value as a Long, if applicable.  Returns
     * None if the value is not present or does not represent a Long.
     */
    def getDouble(key: String): Option[Double] = apply(key) match {
        case JsNumber(n) => Some(n.doubleValue)
        case JsString(str) => 
            try {
                Some(str.toDouble)
            }
            catch {
                case e: NumberFormatException => None
            }
        case _ => None
    }
    
    /**
     * Gets the specified value as a JsArray.  Returns None if the value
     * is not present or is not an array.
     */
    def getArray(key: String): Option[JsArray] = get(key) match {
        case Some(arr: JsArray) => Some(arr)
        case _ => None
    }
    
    /**
     * Gets the specified value as a JsArray.  Returns None if the value
     * is not present or is not an array.
     */
    def getObject(key: String): Option[JsObject] = get(key) match {
        case Some(obj: JsObject) => Some(obj)
        case _ => None
    }
}

object JsObject {
    /**
     * An empty JsObject
     */
    val empty = apply(Map.empty[String,JsValue])
    
    /**
     * An empty JsObject
     */
    def apply(): JsObject = empty
    
    /**
     * Builds an immutable JsObject from a sequence of bindings.
     */
    def apply(bindings: (String,Any)*): JsObject = apply(JsObjectBuffer(bindings: _*))

    /**
     * Builds an immutable JsObject backed by the given map.
     */
    def apply(bindings: Iterable[(String,Any)]): JsObject = {
        apply(JsObjectBuffer(bindings))
    }

    /**
     * Builds an immutable JsObject backed by the given map.
     */
    def apply(bindings: BaseMap[String,JsValue]): JsObject = new JsObject {
        def get(key: String) = bindings.get(key)
        def elements = bindings.elements
        def size = bindings.size
    }
}

/**
 * A mutable JsObject class.  By extending LinkedHashMap, the order in which
 * bindings are added is preserved.
 */
class JsObjectBuffer extends LinkedHashMap[String,JsValue] with JsObject

object JsObjectBuffer {
    /**
     * Builds a mutable JsObjectBuffer from a sequence of bindings.
     */
    def apply(bindings: (String,Any)*): JsObjectBuffer = apply(bindings)

    /**
     * Constructs a JsObjectBuffer from a set of bindings.
     */
    def apply(bindings: Iterable[(String,Any)]): JsObjectBuffer = {
        val obj = new JsObjectBuffer
        bindings.foreach {
            case (k,v) => obj(k) = toJs(v)
        }
        obj
    } 
}

