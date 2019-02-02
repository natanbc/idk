package com.github.natanbc.idk.runtime

final case class IntValue(value: Long) extends Value {
    override def typename: String = "integer"

    override def asLong: Long = value

    override def javaValue: Any = value

    override def isInt: Boolean = true

    override def unary_-(): Value = IntValue(-value)

    override def toString: String = value.toString

    override def isEqual(other: Value): Boolean = (other.isInt && other.asLong == value) || (other.isFloat && other.asDouble == value)

    override def +(other: Value): Value = {
        if(other.isInt) {
            return IntValue(asLong + other.asLong)
        } else if(other.isFloat) {
            return FloatValue(asLong + other.asDouble)
        }
        typeError(s"Attempt to add integer and ${other.typename}")
    }

    override def -(other: Value): Value = {
        if(other.isInt) {
            return IntValue(asLong - other.asLong)
        } else if(other.isFloat) {
            return FloatValue(asLong - other.asDouble)
        }
        typeError(s"Attempt to subtract integer and ${other.typename}")
    }

    override def *(other: Value): Value = {
        if(other.isInt) {
            return IntValue(asLong * other.asLong)
        } else if(other.isFloat) {
            return FloatValue(asLong * other.asDouble)
        }
        typeError(s"Attempt to multiply integer and ${other.typename}")
    }

    override def /(other: Value): Value = {
        if(other.isInt) {
            return IntValue(asLong / other.asLong)
        } else if(other.isFloat) {
            return FloatValue(asLong / other.asDouble)
        }
        typeError(s"Attempt to divide integer and ${other.typename}")
    }

    override def %(other: Value): Value = {
        if(other.isInt) {
            return IntValue(asLong % other.asLong)
        } else if(other.isFloat) {
            return FloatValue(asLong % other.asDouble)
        }
        typeError(s"Attempt to divide integer and ${other.typename}")
    }

    override def ^(other: Value): Value = {
        if(other.isInt) {
            return IntValue(Math.pow(asLong, other.asLong).toLong)
        } else if(other.isFloat) {
            return FloatValue(Math.pow(asLong, other.asDouble))
        }
        typeError(s"Attempt to power integer and ${other.typename}")
    }

    override def >(other: Value): Value = {
        if(other.isInt) {
            return BooleanValue(value > other.asLong)
        } else if(other.isFloat) {
            return BooleanValue(value > other.asDouble)
        }
        typeError(s"Attempt to compare integer and ${other.typename}")
    }

    override def >=(other: Value): Value = {
        if(other.isInt) {
            return BooleanValue(value >= other.asLong)
        } else if(other.isFloat) {
            return BooleanValue(value >= other.asDouble)
        }
        typeError(s"Attempt to compare integer and ${other.typename}")
    }

    override def <(other: Value): Value = {
        if(other.isInt) {
            return BooleanValue(value < other.asLong)
        } else if(other.isFloat) {
            return BooleanValue(value < other.asDouble)
        }
        typeError(s"Attempt to compare integer and ${other.typename}")
    }

    override def <=(other: Value): Value = {
        if(other.isInt) {
            return BooleanValue(value <= other.asLong)
        } else if(other.isFloat) {
            return BooleanValue(value <= other.asDouble)
        }
        typeError(s"Attempt to compare integer and ${other.typename}")
    }

    override def hashCode(): Int = value.hashCode()
}
