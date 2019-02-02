package com.github.natanbc.idk.runtime

sealed abstract class BooleanValue private(val value: Boolean) extends Value {
    override def typename: String = "boolean"

    override def toString: String = value.toString

    override def isBoolean: Boolean = true

    override def asBoolean: Boolean = value

    override def javaValue: Any = value
}

object BooleanValue {
    val True: BooleanValue = TrueValue
    val False: BooleanValue = FalseValue

    def of(b: Boolean): BooleanValue = BooleanValue(b)

    def apply(value: Boolean): BooleanValue = if(value) TrueValue else FalseValue

    def unapply(arg: BooleanValue): Option[Boolean] = Some(arg.value)

    private object TrueValue extends BooleanValue(true) {
        override def unary_!(): Value = FalseValue

        //equal to the default implementation, here just because FalseValue overrides this
        override def ||(other: =>Value): Value = this

        //equal to the default implementation, here just because FalseValue overrides this
        override def &&(other: =>Value): Value = other

        override def isEqual(other: Value): Boolean = (other eq this) || (other.isBoolean && other.asBoolean)

        override def hashCode(): Int = 1
    }

    private object FalseValue extends BooleanValue(false) {
        override def unary_!(): Value = TrueValue

        override def ||(other: =>Value): Value = other

        override def &&(other: =>Value): Value = this

        override def isEqual(other: Value): Boolean = (other eq this) || (other.isBoolean && !other.asBoolean)

        override def hashCode(): Int = 0
    }
}
