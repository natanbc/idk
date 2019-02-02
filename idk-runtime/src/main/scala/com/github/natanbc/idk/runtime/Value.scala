package com.github.natanbc.idk.runtime

import java.lang.reflect.{InvocationTargetException, Modifier}
import java.util
import java.util.function.Supplier

import scala.util.Try

abstract class Value {
    def add(other: Value): Value = this + other
    def +(other: Value): Value = typeError("add", other)

    def subtract(other: Value): Value = this - other
    def -(other: Value): Value = typeError("subtract", other)

    def multiply(other: Value): Value = this * other
    def *(other: Value): Value = typeError("multiply", other)

    def divide(other: Value): Value = this / other
    def /(other: Value): Value = typeError("divide", other)

    def mod(other: Value): Value = this % other
    def %(other: Value): Value = typeError("mod", other)

    def pow(other: Value): Value = this ^ other
    def ^(other: Value): Value = typeError("power", other)

    def neg: Value = -this
    def unary_-(): Value = typeError("negate", null)

    def logical_negate: Value = !this
    def unary_!(): Value = typeError("logical_negate", null)

    def or(other: Supplier[Value]): Value = this || other.get()
    def ||(other: =>Value): Value = this

    def and(other: Supplier[Value]): Value = this && other.get()
    def &&(other: =>Value): Value = other

    def greater(other: Value): Value = this > other
    def >(other: Value): Value = typeError("compare", other)

    def greaterEq(other: Value): Value = this >= other
    def >=(other: Value): Value = typeError("compare", other)

    def smaller(other: Value): Value = this < other
    def <(other: Value): Value = typeError("compare", other)

    def smallerEq(other: Value): Value = this <= other
    def <=(other: Value): Value = typeError("compare", other)

    def call(args: java.util.List[Value]): Value = {
        import collection.JavaConverters._
        apply(args.asScala)
    }
    def apply(args: Seq[Value]): Value = typeError("call", null)

    def typename: String

    def javaValue: Any

    def exactType: String = typename

    def isNumber: Boolean = isInt || isFloat

    def hashCode(): Int

    def isInt: Boolean = false

    def isFloat: Boolean = false

    def isString: Boolean = false

    def isBoolean: Boolean = false

    def isObject: Boolean = false

    def isJavaObject: Boolean = false

    def isFunction: Boolean = false

    def isArray: Boolean = false

    def isNil: Boolean = false

    def asDouble: Double = typeError("not a double")

    def asLong: Long = typeError("not a long")

    def asNumber: Number = if(isInt) asLong else if(isFloat) asDouble else typeError("not a number")

    def asString: String = typeError("not a string")

    def asBoolean: Boolean = typeError("not a boolean")

    def asFunction: Function = typeError("not a function")

    def asMap: collection.mutable.Map[Value, Value] = typeError("not an object")

    def asArray: collection.mutable.ArrayBuffer[Value] = typeError("not an array")

    def asJavaObject: Any = typeError("not a java object")

    def functionName: String = typeError("not a function")

    def length: Long = typeError("not an object or array")

    def isEqual(other: Value): Boolean

    def set(key: Value, value: Value): Value = typeError("not an object or array")

    def get(key: Value): Value = typeError("not an object or array")

    def keys: ArrayValue = typeError("not an object or array")

    override def equals(obj: scala.Any): Boolean = {
        obj.isInstanceOf[Value] && isEqual(obj.asInstanceOf[Value])
    }

    protected def typeError(msg: String): Nothing = throw new TypeError(msg)

    protected def typeError(action: String, other: Value): Nothing = {
        if(other == null) {
            typeError(s"Attempt to $action $typename")
        } else {
            typeError(s"Attempt to $action $typename and ${other.typename}")
        }
    }

    protected def error(msg: String): Nothing = throw new ExecutionException(msg)
}

object Value {
    val defaultWrapper: ObjectWrapper = obj=>new WrappedObject(obj)

    def of(o: Any, wrapper: ObjectWrapper): Value = o match {
        case null => NilValue.instance
        case b: Boolean => BooleanValue(b)
        case i: Byte => IntValue(i)
        case i: Short => IntValue(i)
        case i: Int => IntValue(i)
        case i: Long => IntValue(i)
        case f: Float => FloatValue(f)
        case f: Double => FloatValue(f)
        case c: Char => StringValue(c.toString)
        case s: String => StringValue(s)
        case other => wrapper.wrap(other)
    }

    @inline def of(o: Any): Value = of(o, Value.defaultWrapper)

    private class WrappedObject(instance: Any) extends WrappedJavaValue(instance) {
        private val (methods, fields, constructors) = clazz match {
            case c if c == classOf[Class[_]] =>
                val asClass = instance.asInstanceOf[Class[_]]
                (asClass.getDeclaredMethods ++ asClass.getMethods ++ c.getMethods, asClass.getDeclaredFields, asClass.getDeclaredConstructors)
            case c => (c.getDeclaredMethods ++ c.getMethods, c.getDeclaredFields ++ c.getFields, c.getDeclaredConstructors)
        }
        methods.foreach(_.setAccessible(true))
        fields.foreach(_.setAccessible(true))
        constructors.foreach(_.setAccessible(true))

        private val getMethod = getMethodFunction(false)
        private val getDeclaredMethod = getMethodFunction(true)
        private val getField = getFieldFunction(false)
        private val getDeclaredField = getFieldFunction(true)
        private val getConstructor = getConstructorFunction(false)
        private val getDeclaredConstructor = getConstructorFunction(true)

        private val _keys = ArrayValue()
        _keys.elements.appendAll(Seq(
            "getMethod",
            "getDeclaredMethod",
            "getField",
            "getDeclaredField",
            "getConstructor",
            "getDeclaredConstructor",
        ).map(StringValue))

        override def keys: ArrayValue = _keys

        override def set(key: Value, value: Value): Value = error("unmodifiable object")

        override def get(key: Value): Value = {
            if(key.isString) key.asString match {
                case "getMethod" => return getMethod
                case "getDeclaredMethod" => return getDeclaredMethod
                case "getField" => return getField
                case "getDeclaredField" => return getDeclaredField
                case "getConstructor" => return getConstructor
                case "getDeclaredConstructor" => return getDeclaredConstructor
            }
            NilValue.instance
        }

        private def getMethodFunction(declared: Boolean): Function = new Function {
            override def functionName: String = if(declared) "getDeclaredMethod" else "getMethod"

            override def apply(args: Seq[Value]): Value = {
                if(args.isEmpty) error("Missing method name")
                val name = {
                    val a = args.head
                    if(!a.isString) error("Method name must be a string")
                    a.asString
                }
                val types = args.tail.map(a=>{
                    if(!a.isString) error(s"argument not a string: $a (${a.typename})")
                    a.asString match {
                        case "boolean" => classOf[Boolean]
                        case "byte" => classOf[Byte]
                        case "short" => classOf[Short]
                        case "char" => classOf[Char]
                        case "int" => classOf[Int]
                        case "float" => classOf[Float]
                        case "long" => classOf[Long]
                        case "double" => classOf[Double]
                        case other =>
                            try {
                                Class.forName(other)
                            } catch {
                                case _: ClassNotFoundException => error(s"Class $other not found")
                            }
                    }
                })
                methods.find(m=>{
                    (declared || Modifier.isPublic(m.getModifiers)) && m.getName == name && util.Arrays.equals(m.getParameterTypes.asInstanceOf[Array[AnyRef]], types.toArray.asInstanceOf[Array[AnyRef]])
                }) match {
                    case Some(m) => new Function {
                        private[this] val argTypes = m.getParameterTypes
                        m.setAccessible(true)

                        override def functionName: String = m.getName

                        override def apply(args: Seq[Value]): Value = {
                            val actualArgs = new Array[Object](argTypes.length)

                            val maxIdx = Math.min(args.length, actualArgs.length)
                            for(i <- 0 until maxIdx) {
                                val expected = argTypes(i)
                                val arg = args(i)

                                actualArgs(i) = if(expected.isPrimitive) {
                                    expected.getName match {
                                        case "boolean" => Boolean.box(arg.asBoolean)
                                        case "byte" => Byte.box(arg.asNumber.byteValue())
                                        case "short" => Short.box(arg.asNumber.shortValue())
                                        case "int" => Int.box(arg.asNumber.intValue())
                                        case "float" => Float.box(arg.asNumber.floatValue())
                                        case "long" => Long.box(arg.asNumber.longValue())
                                        case "double" => Double.box(arg.asNumber.doubleValue())
                                        case "char" => Try(Char.box(arg.toString.charAt(0))).getOrElse(throw new ExecutionException("char required, but empty string found"))
                                    }
                                } else {
                                    arg.javaValue.asInstanceOf[Object]
                                }
                            }

                            if(actualArgs.length > maxIdx) {
                                for(i <- maxIdx until actualArgs.length) {
                                    val expected = argTypes(i)
                                    actualArgs(i) = if(expected.isPrimitive) {
                                        expected.getName match {
                                            case "boolean" => Boolean.box(false)
                                            case "byte" => Byte.box(0)
                                            case "short" => Short.box(0)
                                            case "char" => Char.box(0)
                                            case "int" => Int.box(0)
                                            case "float" => Float.box(0)
                                            case "long" => Long.box(0)
                                            case "double" => Double.box(0)
                                        }
                                    } else {
                                        null
                                    }
                                }
                            }

                            try {
                                Value.of(m.invoke(instance, actualArgs: _*))
                            } catch {
                                case e: InvocationTargetException => error(s"error invoking method: ${e.getCause.getMessage}")
                                case e: IllegalArgumentException => typeError(s"error invoking method: ${e.getMessage}")
                            }
                        }
                    }
                    case None => error("Method not found")
                }
            }
        }

        private def getConstructorFunction(declared: Boolean): Function = new Function {
            override def functionName: String = if(declared) "getDeclaredConstructor" else "getConstructor"

            override def apply(args: Seq[Value]): Value = {
                val types = args.map(a=>{
                    if(!a.isString) error(s"argument not a string: $a (${a.typename})")
                    a.asString match {
                        case "boolean" => classOf[Boolean]
                        case "byte" => classOf[Byte]
                        case "short" => classOf[Short]
                        case "char" => classOf[Char]
                        case "int" => classOf[Int]
                        case "float" => classOf[Float]
                        case "long" => classOf[Long]
                        case "double" => classOf[Double]
                        case other =>
                            try {
                                Class.forName(other)
                            } catch {
                                case _: ClassNotFoundException => error(s"Class $other not found")
                            }
                    }
                })
                constructors.find(m=>{
                    (declared || Modifier.isPublic(m.getModifiers)) && util.Arrays.equals(m.getParameterTypes.asInstanceOf[Array[AnyRef]], types.toArray.asInstanceOf[Array[AnyRef]])
                }) match {
                    case Some(m) => new Function {
                        private[this] val argTypes = m.getParameterTypes
                        m.setAccessible(true)

                        override def functionName: String = m.getName

                        override def apply(args: Seq[Value]): Value = {
                            val actualArgs = new Array[Object](argTypes.length)

                            val maxIdx = Math.min(args.length, actualArgs.length)
                            for(i <- 0 until maxIdx) {
                                val expected = argTypes(i)
                                val arg = args(i)

                                actualArgs(i) = if(expected.isPrimitive) {
                                    expected.getName match {
                                        case "boolean" => Boolean.box(arg.asBoolean)
                                        case "byte" => Byte.box(arg.asNumber.byteValue())
                                        case "short" => Short.box(arg.asNumber.shortValue())
                                        case "int" => Int.box(arg.asNumber.intValue())
                                        case "float" => Float.box(arg.asNumber.floatValue())
                                        case "long" => Long.box(arg.asNumber.longValue())
                                        case "double" => Double.box(arg.asNumber.doubleValue())
                                        case "char" => Try(Char.box(arg.toString.charAt(0))).getOrElse(throw new ExecutionException("char required, but empty string found"))
                                    }
                                } else {
                                    arg.javaValue.asInstanceOf[Object]
                                }
                            }

                            if(actualArgs.length > maxIdx) {
                                for(i <- maxIdx until actualArgs.length) {
                                    val expected = argTypes(i)
                                    actualArgs(i) = if(expected.isPrimitive) {
                                        expected.getName match {
                                            case "boolean" => Boolean.box(false)
                                            case "byte" => Byte.box(0)
                                            case "short" => Short.box(0)
                                            case "char" => Char.box(0)
                                            case "int" => Int.box(0)
                                            case "float" => Float.box(0)
                                            case "long" => Long.box(0)
                                            case "double" => Double.box(0)
                                        }
                                    } else {
                                        null
                                    }
                                }
                            }

                            try {
                                Value.of(m.newInstance(actualArgs: _*))
                            } catch {
                                case e: InvocationTargetException => error(s"error invoking constructor: ${e.getCause.getMessage}")
                                case e: IllegalArgumentException => typeError(s"error invoking constructor: ${e.getMessage}")
                            }
                        }
                    }
                    case None => error("Constructor not found")
                }
            }
        }

        private def getFieldFunction(declared: Boolean): Function = new Function {
            override def functionName: String = if(declared) "getDeclaredField" else "getField"

            override def apply(args: Seq[Value]): Value = {
                if(args.isEmpty) error("Missing field name")
                val name = {
                    val a = args.head
                    if(!a.isString) error("Field name must be a string")
                    a.asString
                }
                fields.find(f=>{
                    (declared || Modifier.isPublic(f.getModifiers)) && f.getName == name
                }) match {
                    case Some(f) => new ImmutableObjectValue(Map(
                        StringValue("set") -> new Function {
                            private val expected = f.getType

                            override def functionName: String = "set"

                            override def apply(args: Seq[Value]): Value = {
                                if(args.isEmpty) error("Missing value")
                                val arg = args.head
                                val a = if(expected.isPrimitive) {
                                    expected.getName match {
                                        case "boolean" => Boolean.box(arg.asBoolean)
                                        case "byte" => Byte.box(arg.asNumber.byteValue())
                                        case "short" => Short.box(arg.asNumber.shortValue())
                                        case "int" => Int.box(arg.asNumber.intValue())
                                        case "float" => Float.box(arg.asNumber.floatValue())
                                        case "long" => Long.box(arg.asNumber.longValue())
                                        case "double" => Double.box(arg.asNumber.doubleValue())
                                        case "char" => Try(Char.box(arg.toString.charAt(0))).getOrElse(throw new ExecutionException("char required, but empty string found"))
                                    }
                                } else {
                                    arg.javaValue
                                }
                                try {
                                    f.set(instance, a)
                                } catch {
                                    case e: IllegalArgumentException => error(s"error setting field value: ${e.getMessage}")
                                }
                                NilValue.instance
                            }
                        },
                        StringValue("get") -> new Function {
                            override def functionName: String = "get"

                            override def apply(args: Seq[Value]): Value = {
                                Value.of(f.get(instance))
                            }
                        }
                    ))
                    case None => error("Field not found")
                }
            }
        }
    }

    private class ImmutableObjectValue(m: Map[Value, Value]) extends Value {
        override def typename: String = "object"

        override def isObject: Boolean = true

        override def isEqual(other: Value): Boolean = false

        override def javaValue: Any = null

        override def keys: ArrayValue = {
            val a = ArrayValue()
            a.elements.appendAll(m.keys)
            a
        }

        override def set(key: Value, value: Value): Value = error("immutable")

        override def get(key: Value): Value = m.getOrElse(key, NilValue.instance)

        override def hashCode(): Int = m.hashCode()
    }
}
