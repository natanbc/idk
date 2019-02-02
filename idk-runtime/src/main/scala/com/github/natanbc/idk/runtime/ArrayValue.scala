package com.github.natanbc.idk.runtime

import scala.collection.mutable.ArrayBuffer

final case class ArrayValue(elements: ArrayBuffer[Value] = new ArrayBuffer()) extends Value {
    override def typename: String = "array"

    override def javaValue: Any = elements

    override def isArray: Boolean = true

    override def asArray: ArrayBuffer[Value] = elements

    override def isEqual(other: Value): Boolean = other.isArray && other.asArray == elements

    override def length: Long = elements.length

    override def set(key: Value, value: Value): Value = {
        if(!key.isInt) typeError(s"Attempt to write to non int index $key")
        val k = key.asLong
        if(k < 0) error("Negative array index")
        if(k > Integer.MAX_VALUE) error("Array index too large")
        elements.insert(k.toInt, value)
        value
    }

    override def get(key: Value): Value = {
        if(!key.isInt) typeError(s"Attempt to read non int index $key")
        val k = key.asLong
        if(k < 0) error("Negative array index")
        if(k > Integer.MAX_VALUE) error("Array index too large")
        if(elements.length <= k) {
            NilValue.instance
        } else {
            val v = elements(k.toInt)
            if(v == null) {
                NilValue.instance
            } else {
                v
            }
        }
    }

    override def toString: String = elements.mkString("[", ", ", "]")

    override def hashCode(): Int = elements.hashCode()
}
