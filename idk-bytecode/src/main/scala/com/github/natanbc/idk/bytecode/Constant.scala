package com.github.natanbc.idk.bytecode

class Constant {
    def isString: Boolean = false
    def asString: String = throw new IllegalArgumentException("not a string")

    def isInt: Boolean = false
    def asLong: Long = throw new IllegalArgumentException("not a long")

    def isFloat: Boolean = false
    def asDouble: Double = throw new IllegalArgumentException("not a double")
}

case class StringConstant(s: String) extends Constant {
    override def isString: Boolean = true

    override def asString: String = s
}

case class IntConstant(l: Long) extends Constant {
    override def isInt: Boolean = true

    override def asLong: Long = l
}

case class FloatConstant(d: Double) extends Constant {
    override def isFloat: Boolean = true

    override def asDouble: Double = d
}
