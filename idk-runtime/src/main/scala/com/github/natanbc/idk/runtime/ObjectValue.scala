package com.github.natanbc.idk.runtime

import scala.collection.mutable

final case class ObjectValue(map: mutable.Map[Value, Value] = new mutable.HashMap()) extends Value {
    private val _keys = ArrayValue()

    override def typename: String = "object"

    override def isObject: Boolean = true

    override def asMap: mutable.Map[Value, Value] = map

    override def javaValue: Any = map

    override def isEqual(other: Value): Boolean = other.isObject && other.asMap == map

    override def length: Long = map.size

    override def set(key: Value, value: Value): Value = {
        if(value eq NilValue.instance) {
            map.remove(key)
            val i = keys.elements.indexOf(key)
            if(i != -1) keys.elements.remove(i)
        } else {
            map.put(key, value)
            if(!keys.elements.contains(key)) keys.elements.append(key)
        }
        value
    }

    override def get(key: Value): Value = {
        map.getOrElse(key, NilValue.instance)
    }

    @inline override def keys: ArrayValue = _keys

    override def toString: String = map.toSeq.map(p=>s"${p._1} = ${p._2}").mkString("{", ", ", "}")

    override def hashCode(): Int = map.hashCode()
}
