package com.github.natanbc.idk.runtime

object StdLib {
    def install(map: collection.mutable.Map[Value, Value]): Unit = {
        map.put(StringValue("print"), new Function {
            override def apply(args: Seq[Value]): Value = {
                args.foreach(println)
                NilValue.instance
            }

            override def functionName: String = "print"
        })

        map.put(StringValue("create_object"), new Function {
            override def apply(args: Seq[Value]): Value = {
                if(args.length % 2 != 0) error("uneven argument number")
                val v = ObjectValue()
                args.grouped(2).map(l=>(l.head, l.tail.head)).foreach(p=>v.set(p._1, p._2))
                v
            }

            override def functionName: String = "create_object"
        })

        map.put(StringValue("type"), new Function {
            override def apply(args: Seq[Value]): Value = {
                args match {
                    case thing :: BooleanValue(v) :: _ => StringValue(if(v) thing.exactType else thing.typename)
                    case thing :: _ => StringValue(thing.typename)
                    case _ => error("missing argument")
                }
            }

            override def functionName: String = "type"
        })

        map.put(StringValue("keys"), new Function {
            override def apply(args: Seq[Value]): Value = {
                if(args.isEmpty) error("missing argument")
                val o = args.head
                if(o.isObject || o.isJavaObject) {
                    return o.keys
                }
                error(s"object expected, got ${o.typename}")
            }

            override def functionName: String = "keys"
        })

        map.put(StringValue("size"), new Function {
            override def apply(args: Seq[Value]): Value = {
                if(args.isEmpty) error("missing argument")
                val o = args.head
                if(o.isObject) {
                    return IntValue(o.asMap.size)
                } else if(o.isArray) {
                    return IntValue(o.asArray.size)
                }
                error(s"object or array expected, got ${o.typename}")
            }

            override def functionName: String = "size"
        })
    }
}
