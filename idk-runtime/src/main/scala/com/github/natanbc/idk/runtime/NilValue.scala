package com.github.natanbc.idk.runtime

class NilValue private() extends Value {
    override def typename: String = "nil"

    override def toString: String = "nil"

    override def javaValue: Any = null

    override def isNil: Boolean = true

    override def ||(other: =>Value): Value = other

    override def &&(other: =>Value): Value = this

    override def isEqual(other: Value): Boolean = other.isNil

    override def hashCode(): Int = 0
}

object NilValue {
    final val instance = new NilValue()
}