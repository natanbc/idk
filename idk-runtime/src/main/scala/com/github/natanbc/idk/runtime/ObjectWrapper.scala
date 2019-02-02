package com.github.natanbc.idk.runtime

trait ObjectWrapper {
    def wrap(o: Any): WrappedJavaValue
}
