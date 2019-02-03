package com.github.natanbc.idk.runtime

final case class StringValue(value: String) extends Value {
    override def +(other: Value): Value = StringValue(value + other.toString)

    override def *(other: Value): Value = {
        if(other.isInt) {
            val v = other.asLong
            if(v < 0 || v > Integer.MAX_VALUE) {
                error("Value out of bounds")
            }
            return StringValue(value * v.toInt)
        }
        error(s"Attempt to multiply string and ${other.typename}")
    }

    override def typename: String = "string"

    override def toString: String = value

    override def isString: Boolean = true

    override def asString: String = value

    override def javaValue: Any = value

    override def isEqual(other: Value): Boolean = other.isString && other.asString == value

    override def hashCode(): Int = value.hashCode

    override def get(key: Value): Value = {
        if(key.isInt) {
            val k = key.asLong
            if(k < 0) error("Negative index")
            if(k >= value.length) {
                NilValue.instance
            } else {
                StringValue(value.charAt(k.toInt).toString)
            }
        } else {
            typeError(s"Attempt to read non int index $key")
        }
    }
}
