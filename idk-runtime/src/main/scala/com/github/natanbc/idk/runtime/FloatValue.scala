package com.github.natanbc.idk.runtime

final case class FloatValue(value: Double) extends Value {
    override def typename: String = "float"

    override def asDouble: Double = value

    override def javaValue: Any = value

    override def isFloat: Boolean = true

    override def unary_-(): Value = FloatValue(-value)

    override def toString: String = value.toString

    override def isEqual(other: Value): Boolean = (other.isFloat && other.asDouble == value) || (other.isInt && other.asLong == value)

    override def +(other: Value): Value = {
        if(other.isInt) {
            return FloatValue(asDouble + other.asLong)
        } else if(other.isFloat) {
            return FloatValue(asDouble + other.asDouble)
        }
        typeError(s"Attempt to add float and ${other.typename}")
    }

    override def -(other: Value): Value = {
        if(other.isInt) {
            return FloatValue(asDouble - other.asLong)
        } else if(other.isFloat) {
            return FloatValue(asDouble - other.asDouble)
        }
        typeError(s"Attempt to subtract float and ${other.typename}")
    }

    override def *(other: Value): Value = {
        if(other.isInt) {
            return FloatValue(asDouble * other.asLong)
        } else if(other.isFloat) {
            return FloatValue(asDouble * other.asDouble)
        }
        typeError(s"Attempt to multiply float and ${other.typename}")
    }

    override def %(other: Value): Value = {
        if(other.isInt) {
            return FloatValue(asDouble % other.asLong)
        } else if(other.isFloat) {
            return FloatValue(asDouble % other.asDouble)
        }
        typeError(s"Attempt to divide integer and ${other.typename}")
    }

    override def /(other: Value): Value = {
        if(other.isInt) {
            return FloatValue(asDouble / other.asLong)
        } else if(other.isFloat) {
            return FloatValue(asDouble / other.asDouble)
        }
        typeError(s"Attempt to divide float and ${other.typename}")
    }

    override def ^(other: Value): Value = {
        if(other.isInt) {
            return FloatValue(Math.pow(asDouble, other.asLong))
        } else if(other.isFloat) {
            return FloatValue(Math.pow(asDouble, other.asDouble))
        }
        typeError(s"Attempt to power float and ${other.typename}")
    }

    override def >(other: Value): Value = {
        if(other.isInt) {
            return BooleanValue(value > other.asLong)
        } else if(other.isFloat) {
            return BooleanValue(value > other.asDouble)
        }
        typeError(s"Attempt to compare float and ${other.typename}")
    }

    override def >=(other: Value): Value = {
        if(other.isInt) {
            return BooleanValue(value >= other.asLong)
        } else if(other.isFloat) {
            return BooleanValue(value >= other.asDouble)
        }
        typeError(s"Attempt to compare float and ${other.typename}")
    }

    override def <(other: Value): Value = {
        if(other.isInt) {
            return BooleanValue(value < other.asLong)
        } else if(other.isFloat) {
            return BooleanValue(value < other.asDouble)
        }
        typeError(s"Attempt to compare float and ${other.typename}")
    }

    override def <=(other: Value): Value = {
        if(other.isInt) {
            return BooleanValue(value <= other.asLong)
        } else if(other.isFloat) {
            return BooleanValue(value <= other.asDouble)
        }
        typeError(s"Attempt to compare float and ${other.typename}")
    }

    override def hashCode(): Int = value.hashCode()
}
