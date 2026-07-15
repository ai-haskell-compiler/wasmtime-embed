(module
  (import "host" "log" (func $host_log))

  (func (export "double") (param i32) (result i32)
    local.get 0
    i32.const 2
    i32.mul
  )

  (func (export "log") (param i32 i32)
    call $host_log
  )

  (memory (export "memory") 2)
  (global (export "memory_offset") i32 (i32.const 65536))
)
