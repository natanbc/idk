package com.github.natanbc.idk.runtime

import java.io.ByteArrayInputStream
import java.util

import com.github.natanbc.idk.bytecode.{BytecodeReader, CompiledFunction, Opcodes}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Interpreter(_bytecode: Array[Byte], val globals: mutable.Map[Value, Value]) {
    def this(bytecode: Array[Byte]) = {
        this(bytecode, new mutable.HashMap[Value, Value]())
    }

    def bytecode: Array[Byte] = _bytecode.clone()

    private val br = new BytecodeReader(new ByteArrayInputStream(_bytecode))

    private val functions: Map[Int, CompiledFunction] = {
        val b = new mutable.HashMap[Int, CompiledFunction]()
        var f = br.readFunction()
        while(f != null) {
            b(f.id) = f
            f = br.readFunction()
        }
        b.toMap
    }

    private val callStack = new mutable.ListBuffer[String]()

    class FunctionInterpreter private[Interpreter](private val f: CompiledFunction, private val parent: FunctionInterpreter = null) {
        //array has fast random access
        private val locals = Array.fill[Value](f.localCount)(NilValue.instance)
        private val code = f.code.toArray
        private[Interpreter] val stack = new util.Stack[Value]()
        private var ip = 0

        def hasNext: Boolean = ip < code.length

        def next(): Unit = {
            if(ip >= code.length) {
                throw new IllegalStateException("Execution already finished")
            }
            val op = code(ip)
            ip += 1
            op.opcode match {
                case Opcodes.LOAD_STRING_CONSTANT => stack.push(StringValue(br.getStringConstant(op.args.head)))
                case Opcodes.LOAD_LONG_CONSTANT => stack.push(IntValue(br.getLongConstant(op.args.head)))
                case Opcodes.LOAD_DOUBLE_CONSTANT => stack.push(FloatValue(br.getDoubleConstant(op.args.head)))
                case Opcodes.LOAD_TRUE => stack.push(BooleanValue.True)
                case Opcodes.LOAD_FALSE => stack.push(BooleanValue.False)
                case Opcodes.LOAD_NIL => stack.push(NilValue.instance)

                case Opcodes.ADD =>
                    val a = stack.pop()
                    stack.push(stack.pop() + a)
                case Opcodes.SUB =>
                    val a = stack.pop()
                    stack.push(stack.pop() - a)
                case Opcodes.MUL =>
                    val a = stack.pop()
                    stack.push(stack.pop() * a)
                case Opcodes.DIV =>
                    val a = stack.pop()
                    stack.push(stack.pop() / a)
                case Opcodes.MOD =>
                    val a = stack.pop()
                    stack.push(stack.pop() % a)
                case Opcodes.POW =>
                    val a = stack.pop()
                    stack.push(stack.pop() ^ a)

                case Opcodes.NEG =>
                    stack.push(-stack.pop())
                case Opcodes.NEGATE =>
                    stack.push(!stack.pop())

                case Opcodes.EQ =>
                    stack.push(BooleanValue(stack.pop() isEqual stack.pop()))
                case Opcodes.NEQ =>
                    stack.push(BooleanValue(!(stack.pop() isEqual stack.pop())))
                case Opcodes.GREATER =>
                    val a = stack.pop()
                    stack.push(stack.pop() > a)
                case Opcodes.GREATER_EQ =>
                    val a = stack.pop()
                    stack.push(stack.pop() >= a)
                case Opcodes.SMALLER =>
                    val a = stack.pop()
                    stack.push(stack.pop() < a)
                case Opcodes.SMALLER_EQ =>
                    val a = stack.pop()
                    stack.push(stack.pop() <= a)

                case Opcodes.AND =>
                    stack.push(stack.pop() && stack.pop())
                case Opcodes.OR =>
                    stack.push(stack.pop() || stack.pop())

                case Opcodes.ARRAY_LITERAL =>
                    val b = new Array[Value](op.args.head)
                    for(i <- op.args.head to 1) {
                        b(i - 1) = stack.pop()
                    }
                    stack.push(ArrayValue(b.to[ArrayBuffer]))

                case Opcodes.OBJECT_LITERAL =>
                    val m = new mutable.HashMap[Value, Value]()
                    m.sizeHint(op.args.head)
                    for(_ <- 1 to op.args.head) {
                        val v = stack.pop()
                        val k = stack.pop()
                        if(!m.isDefinedAt(k)) {
                            m.put(k, v)
                        }
                    }
                    stack.push(ObjectValue(m))

                case Opcodes.DUP =>
                    stack.push(stack.peek())

                case Opcodes.IF =>
                    if(stack.pop().asBoolean) {
                        ip = op.args.head
                    }

                case Opcodes.GOTO =>
                    ip = op.args.head

                case Opcodes.LOAD_GLOBAL =>
                    stack.push(globals.getOrElse(StringValue(br.getStringConstant(op.args.head)), NilValue.instance))
                case Opcodes.STORE_GLOBAL =>
                    globals.put(StringValue(br.getStringConstant(op.args.head)), stack.peek())

                case Opcodes.LOAD_LOCAL =>
                    stack.push(locals(op.args.head))
                case Opcodes.STORE_LOCAL =>
                    locals(op.args.head) = stack.peek()

                case Opcodes.LOAD_FIELD =>
                    val k = stack.pop()
                    stack.push(stack.pop().get(k))
                case Opcodes.STORE_FIELD =>
                    val v = stack.pop()
                    val k = stack.pop()
                    stack.push(stack.pop().set(k, v))

                case Opcodes.LOAD_UPVALUE =>
                    val parentIdx = op.args.head
                    val localIdx = op.args(1)
                    var p = this
                    for(_ <- 1 to parentIdx) {
                        p = p.parent
                    }
                    stack.push(p.locals(localIdx))
                case Opcodes.STORE_UPVALUE =>
                    val parentIdx = op.args.head
                    val localIdx = op.args(1)
                    var p = this
                    for(_ <- 1 to parentIdx) {
                        p = p.parent
                    }
                    p.locals(localIdx) = stack.peek()

                case Opcodes.POP => stack.pop()

                case Opcodes.CALL =>
                    val a = new mutable.ListBuffer[Value]()
                    a.sizeHint(op.args.head)
                    for(_ <- 1 to op.args.head) {
                        a.insert(0, stack.pop())
                    }
                    val v = stack.pop()
                    if(v.isFunction) {
                        callStack.append(v.functionName)
                    }
                    try {
                        stack.push(v(a.toList))
                    } catch {
                        case e: ExecutionException =>
                            if(e.callStack == null) e.callStack = callStack.toList.reverse
                            throw e
                    } finally {
                        callStack.remove(callStack.length - 1)
                    }
                case Opcodes.UNPACK_CALL =>
                    val a = new mutable.ListBuffer[Value]()
                    a.sizeHint(op.args.head)
                    //exclude last
                    val l = stack.pop()
                    for(_ <- 1 until op.args.head) {
                        a.insert(0, stack.pop())
                    }
                    if(!l.isArray) {
                        throw new ExecutionException("Last argument in an unpack call must be an array")
                    }
                    a.appendAll(l.asArray)
                    val v = stack.pop()
                    if(v.isFunction) {
                        callStack.append(v.functionName)
                    }
                    try {
                        stack.push(v(a.toList))
                    } catch {
                        case e: ExecutionException =>
                            if(e.callStack == null) e.callStack = callStack.toList.reverse
                            throw e
                    } finally {
                        callStack.remove(callStack.length - 1)
                    }

                case Opcodes.DECORATOR_CALL =>
                    val fn = stack.pop()
                    val decorator = stack.pop()
                    if(!decorator.isFunction) {
                        throw new ExecutionException(s"Decorator ${br.getStringConstant(op.args.head)} is not a function!")
                    }
                    callStack.append(decorator.functionName)
                    try {
                        val v = decorator.apply(List(fn))
                        if(!v.isFunction) {
                            throw new ExecutionException(s"Decorator ${br.getStringConstant(op.args.head)} didn't return a function!")
                        }
                        v.asFunction.annotations = fn.asFunction.annotations
                        stack.push(v)
                    } catch {
                        case e: ExecutionException =>
                            if(e.callStack == null) e.callStack = callStack.toList.reverse
                            throw e
                    } finally {
                        callStack.remove(callStack.length - 1)
                    }

                case Opcodes.LOAD_FUNCTION =>
                    val function = functions(op.args.head)
                    val fn = new Function {
                        override def javaValue: Any = ()=>new FunctionInterpreter(function, FunctionInterpreter.this)

                        override def apply(args: Seq[Value]): Value = {
                            val interpreter = new FunctionInterpreter(function, FunctionInterpreter.this)
                            args.take(function.argCount - (if(function.varargs) 1 else 0)).zipWithIndex.foreach(p=>{
                                interpreter.locals(p._2) = p._1
                            })
                            if(function.varargs && function.argCount > 0) {
                                interpreter.locals(function.argCount - 1) = ArrayValue(args.drop(function.argCount - 1).to[ArrayBuffer])
                            }
                            while(interpreter.hasNext) interpreter.next()
                            if(interpreter.stack.isEmpty) {
                                NilValue.instance
                            } else {
                                interpreter.stack.peek()
                            }
                        }

                        override def functionName: String = if(function.name == null) "<anonymous>" else function.name

                        override def toString: String = if(function.name == null) f"function: 0x${hashCode()}%8x" else s"function: ${function.name}"
                    }
                    fn.annotations = function.annotations
                    stack.push(fn)

                case Opcodes.RETURN =>
                    ip = code.length

                case _ => throw new UnsupportedOperationException("opcode " + op.opcode.id + " (" + Opcodes.nameOf(op.opcode.id) + ")")
            }
        }
    }

    def run(): Value = {
        functions.values.find(_.name == "$main") match {
            case Some(function) =>
                if(callStack.isEmpty) {
                    callStack.append("<main>")
                }
                val f = new FunctionInterpreter(function)
                try {
                    while(f.hasNext) f.next()
                } catch {
                    case e: ExecutionException =>
                        if(e.callStack == null) e.callStack = callStack.reverse.toList
                        throw e
                }
                if(callStack.length == 1) {
                    callStack.remove(0)
                }
                if(f.stack.isEmpty) {
                    NilValue.instance
                } else {
                    f.stack.peek()
                }
            case None => throw new IllegalStateException("no main function")
        }
    }
}
