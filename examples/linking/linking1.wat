(module
  (import "linking2" "double" (func $double (param i32) (result i32)))
  (import "linking2" "log" (func $log (param i32 i32)))
  (import "linking2" "memory" (memory 1))
  (import "linking2" "memory_offset" (global $offset i32))

  (func (export "run")
    i32.const 2
    call $double
    drop

    global.get $offset
    i32.const 14
    call $log
  )

  (data (global.get $offset) "Hello, world!\n")
)
