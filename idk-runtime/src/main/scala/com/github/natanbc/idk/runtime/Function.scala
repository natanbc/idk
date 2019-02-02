package com.github.natanbc.idk.runtime

import java.util

import scala.collection.mutable.ArrayBuffer

abstract class Function extends Value {
    private var _annotations: Seq[String] = Seq()

    override def typename: String = "function"

    override def isFunction: Boolean = true

    override def asFunction: Function = this

    override def javaValue: Any = this

    override def isEqual(other: Value): Boolean = false

    override def toString: String = s"function: $functionName"

    override def hashCode(): Int = System.identityHashCode(this)

    override def get(key: Value): Value = {
        if(key.isString) key.asString match {
            case "annotations" =>
                return ArrayValue(_annotations.map(Value.of).to[ArrayBuffer])
            case _ =>
        }
        NilValue.instance
    }

    override def set(key: Value, value: Value): Value = {
        throw new ExecutionException("functions are immutable")
    }

    def annotations: Seq[String] = _annotations

    def annotations_=(a: Seq[String]): Unit = _annotations = a

    def functionName: String

    def apply(args: Seq[Value]): Value
}

object Function {
    abstract class Java extends Function {
        import collection.JavaConverters._

        override def apply(args: Seq[Value]): Value = {
            call(args.asJava)
        }

        def call(args: util.List[Value]): Value

        def getAnnotations: util.List[String] = annotations.asJava

        def setAnnotations(a: util.List[String]): Unit = annotations = a.asScala.toList
    }
}
