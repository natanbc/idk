package com.github.natanbc.idk.runtime

abstract class WrappedJavaValue(instance: Any) extends Value {
    val clazz: Class[_] = instance.getClass

    def keys: ArrayValue

    override def isJavaObject: Boolean = true

    override def toString: String = instance.toString

    override def typename: String = "object"

    override def javaValue: Any = instance

    override def exactType: String = clazz.getName

    override def asJavaObject: Any = instance

    override def isEqual(other: Value): Boolean = other.isJavaObject && other.asJavaObject == instance

    override def hashCode(): Int = instance.hashCode()
}
