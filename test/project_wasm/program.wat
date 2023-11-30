(module
 (type $0 (struct (field i32)))
 (type $1 (struct (field i64)))
 (type $2 (sub (struct (field i32) (field (ref null $1)) (field (ref null $1)))))
 (type $3 (struct (field f64)))
 (type $4 (sub final $2 (struct (field i32) (field (ref null $1)) (field (ref null $1)))))
 (type $5 (sub (struct (field i32) (field (ref null $1)))))
 (type $6 (sub (struct (field i32) (field (ref null $1)) (field (ref null $5)))))
 (type $7 (sub final $6 (struct (field i32) (field (ref null $1)) (field (ref null $5)))))
 (type $8 (sub final $5 (struct (field i32) (field (ref null $1)))))
 (type $9 (struct (field (ref null $9)) (field anyref)))
 (type $10 (func (param (ref null $1) (ref null $1)) (result (ref null $2))))
 (type $11 (func (param (ref null $1)) (result (ref null $5))))
 (type $12 (func (param (ref null $1) (ref null $5)) (result (ref null $6))))
 (type $13 (struct (field (ref null $1)) (field (ref null $1))))
 (type $14 (func (result (ref null $0))))
 (type $15 (func (param (ref null $2) (ref null $2)) (result (ref null $2))))
 (type $16 (func (param i64) (result (ref null $1))))
 (type $17 (func (param (ref null $1) (ref null $1)) (result i32)))
 (type $18 (func (param (ref null $1)) (result i64)))
 (type $19 (func (param f64) (result (ref null $3))))
 (type $20 (func (param (ref null $3) (ref null $3)) (result i32)))
 (type $21 (func (param (ref null $3)) (result f64)))
 (type $22 (func (param (ref null $0) (ref null $0)) (result i32)))
 (type $23 (func (param (ref null $0)) (result i32)))
 (type $24 (func (param (ref null $2) (ref null $2)) (result i32)))
 (type $25 (func (param (ref null $5) (ref null $5)) (result i32)))
 (type $26 (func (param (ref null $6) (ref null $6)) (result i32)))
 (type $27 (func (result (ref null $1))))
 (type $28 (array i8))
 (memory $0 1)
 (data $0 (i32.const 0) "hello world")
 (export "gleam/Int$eq" (func $1))
 (export "gleam/Int$to_int" (func $2))
 (export "gleam/Float$eq" (func $4))
 (export "gleam/Float$to_float" (func $5))
 (export "gleam/Bool$eq" (func $8))
 (export "gleam/Bool$to_bool" (func $9))
 (export "project_wasm/pair/Pair$eq" (func $10))
 (export "project_wasm/pair/add" (func $11))
 (export "project_wasm/Foo$eq" (func $12))
 (export "project_wasm/Bar$eq" (func $13))
 (export "project_wasm/main" (func $14))
 (export "project_wasm/pair/Pair" (func $15))
 (export "project_wasm/Foo" (func $16))
 (export "project_wasm/Bar" (func $17))
 (func $0 (param $0 i64) (result (ref null $1))
  (struct.new $1
   (local.get $0)
  )
 )
 (func $1 (param $0 (ref null $1)) (param $1 (ref null $1)) (result i32)
  (i64.eq
   (struct.get $1 0
    (local.get $0)
   )
   (struct.get $1 0
    (local.get $1)
   )
  )
 )
 (func $2 (param $0 (ref null $1)) (result i64)
  (struct.get $1 0
   (local.get $0)
  )
 )
 (func $3 (param $0 f64) (result (ref null $3))
  (struct.new $3
   (local.get $0)
  )
 )
 (func $4 (param $0 (ref null $3)) (param $1 (ref null $3)) (result i32)
  (f64.eq
   (struct.get $3 0
    (local.get $0)
   )
   (struct.get $3 0
    (local.get $1)
   )
  )
 )
 (func $5 (param $0 (ref null $3)) (result f64)
  (struct.get $3 0
   (local.get $0)
  )
 )
 (func $6 (result (ref null $0))
  (struct.new $0
   (i32.const 1)
  )
 )
 (func $7 (result (ref null $0))
  (struct.new $0
   (i32.const 0)
  )
 )
 (func $8 (param $0 (ref null $0)) (param $1 (ref null $0)) (result i32)
  (i32.eq
   (struct.get $0 0
    (local.get $0)
   )
   (struct.get $0 0
    (local.get $1)
   )
  )
 )
 (func $9 (param $0 (ref null $0)) (result i32)
  (struct.get $0 0
   (local.get $0)
  )
 )
 (func $10 (param $0 (ref null $2)) (param $1 (ref null $2)) (result i32)
  (local $2 (ref null $4))
  (local $3 (ref null $4))
  (if
   (i32.ne
    (struct.get $2 0
     (local.get $0)
    )
    (struct.get $2 0
     (local.get $1)
    )
   )
   (return
    (i32.const 0)
   )
  )
  (block $label$2
   (block $label$3
    (br_table $label$3 $label$2
     (struct.get $2 0
      (local.get $0)
     )
    )
   )
   (local.set $2
    (ref.cast (ref null $4)
     (local.get $0)
    )
   )
   (local.set $3
    (ref.cast (ref null $4)
     (local.get $1)
    )
   )
   (if
    (i32.eqz
     (call $1
      (struct.get $4 1
       (local.get $2)
      )
      (struct.get $4 1
       (local.get $3)
      )
     )
    )
    (return
     (i32.const 0)
    )
   )
   (if
    (i32.eqz
     (call $1
      (struct.get $4 2
       (local.get $2)
      )
      (struct.get $4 2
       (local.get $3)
      )
     )
    )
    (return
     (i32.const 0)
    )
   )
   (return
    (i32.const 1)
   )
  )
  (i32.const 1)
 )
 (func $11 (param $0 (ref null $2)) (param $1 (ref null $2)) (result (ref null $2))
  (call_ref $10
   (call $0
    (i64.add
     (struct.get $1 0
      (struct.get $2 1
       (local.get $0)
      )
     )
     (struct.get $1 0
      (struct.get $2 1
       (local.get $1)
      )
     )
    )
   )
   (call $0
    (i64.add
     (struct.get $1 0
      (struct.get $2 2
       (local.get $0)
      )
     )
     (struct.get $1 0
      (struct.get $2 2
       (local.get $1)
      )
     )
    )
   )
   (ref.func $15)
  )
 )
 (func $12 (param $0 (ref null $5)) (param $1 (ref null $5)) (result i32)
  (local $2 (ref null $8))
  (local $3 (ref null $8))
  (if
   (i32.ne
    (struct.get $5 0
     (local.get $0)
    )
    (struct.get $5 0
     (local.get $1)
    )
   )
   (return
    (i32.const 0)
   )
  )
  (block $label$2
   (block $label$3
    (br_table $label$3 $label$2
     (struct.get $5 0
      (local.get $0)
     )
    )
   )
   (local.set $2
    (ref.cast (ref null $8)
     (local.get $0)
    )
   )
   (local.set $3
    (ref.cast (ref null $8)
     (local.get $1)
    )
   )
   (if
    (i32.eqz
     (call $1
      (struct.get $8 1
       (local.get $2)
      )
      (struct.get $8 1
       (local.get $3)
      )
     )
    )
    (return
     (i32.const 0)
    )
   )
   (return
    (i32.const 1)
   )
  )
  (i32.const 1)
 )
 (func $13 (param $0 (ref null $6)) (param $1 (ref null $6)) (result i32)
  (local $2 (ref null $7))
  (local $3 (ref null $7))
  (if
   (i32.ne
    (struct.get $6 0
     (local.get $0)
    )
    (struct.get $6 0
     (local.get $1)
    )
   )
   (return
    (i32.const 0)
   )
  )
  (block $label$2
   (block $label$3
    (br_table $label$3 $label$2
     (struct.get $6 0
      (local.get $0)
     )
    )
   )
   (local.set $2
    (ref.cast (ref null $7)
     (local.get $0)
    )
   )
   (local.set $3
    (ref.cast (ref null $7)
     (local.get $1)
    )
   )
   (if
    (i32.eqz
     (call $1
      (struct.get $7 1
       (local.get $2)
      )
      (struct.get $7 1
       (local.get $3)
      )
     )
    )
    (return
     (i32.const 0)
    )
   )
   (if
    (i32.eqz
     (call $12
      (struct.get $7 2
       (local.get $2)
      )
      (struct.get $7 2
       (local.get $3)
      )
     )
    )
    (return
     (i32.const 0)
    )
   )
   (return
    (i32.const 1)
   )
  )
  (i32.const 1)
 )
 (func $14 (result (ref null $1))
  (local $0 (ref null $2))
  (local $1 (ref null $2))
  (local $2 (ref null $6))
  (local $3 (ref null $6))
  (drop
   (local.tee $0
    (block $label$1 (result (ref null $2))
     (drop
      (local.tee $1
       (call_ref $10
        (call $0
         (i64.const 2)
        )
        (call $0
         (i64.const 8)
        )
        (ref.func $15)
       )
      )
     )
     (call_ref $15
      (local.get $1)
      (call_ref $10
       (call $0
        (i64.const 4)
       )
       (call $0
        (i64.const 8)
       )
       (ref.func $15)
      )
      (ref.func $11)
     )
    )
   )
  )
  (drop
   (local.tee $2
    (call_ref $12
     (call $0
      (i64.const 4)
     )
     (call_ref $11
      (call $0
       (i64.const 6)
      )
      (ref.func $16)
     )
     (ref.func $17)
    )
   )
  )
  (drop
   (local.tee $3
    (call_ref $12
     (call $0
      (i64.const 4)
     )
     (call_ref $11
      (call $0
       (i64.const 6)
      )
      (ref.func $16)
     )
     (ref.func $17)
    )
   )
  )
  (drop
   (call $0
    (i64.add
     (struct.get $1 0
      (struct.get $2 1
       (local.get $0)
      )
     )
     (struct.get $1 0
      (struct.get $2 2
       (local.get $0)
      )
     )
    )
   )
  )
  (drop
   (call $3
    (f64.add
     (struct.get $3 0
      (call $3
       (f64.const 4.5)
      )
     )
     (struct.get $3 0
      (call $3
       (f64.const 1.25)
      )
     )
    )
   )
  )
  (drop
   (call $6)
  )
  (drop
   (call $7)
  )
  (drop
   (struct.new $0
    (i64.gt_s
     (struct.get $1 0
      (call $0
       (i64.const 2)
      )
     )
     (struct.get $1 0
      (call $0
       (i64.const 3)
      )
     )
    )
   )
  )
  (drop
   (struct.new $0
    (f64.gt
     (struct.get $3 0
      (call $3
       (f64.const 2)
      )
     )
     (struct.get $3 0
      (call $3
       (f64.const 3)
      )
     )
    )
   )
  )
  (drop
   (struct.new $0
    (i32.eqz
     (call $1
      (call $0
       (i64.const 1)
      )
      (call $0
       (i64.const 3)
      )
     )
    )
   )
  )
  (drop
   (struct.new $0
    (call $8
     (call $6)
     (call $6)
    )
   )
  )
  (drop
   (struct.new $0
    (call $8
     (call $6)
     (call $7)
    )
   )
  )
  (drop
   (struct.new $0
    (i32.eqz
     (call $8
      (call $6)
      (call $6)
     )
    )
   )
  )
  (drop
   (struct.new $0
    (i32.eqz
     (call $8
      (call $6)
      (call $7)
     )
    )
   )
  )
  (drop
   (struct.new $0
    (call $4
     (call $3
      (f64.const 1)
     )
     (call $3
      (f64.const 1)
     )
    )
   )
  )
  (drop
   (struct.new $0
    (call $4
     (call $3
      (f64.const 1)
     )
     (call $3
      (f64.const 2)
     )
    )
   )
  )
  (drop
   (struct.new $0
    (i32.eqz
     (call $4
      (call $3
       (f64.const 1)
      )
      (call $3
       (f64.const 1)
      )
     )
    )
   )
  )
  (drop
   (struct.new $0
    (i32.eqz
     (call $4
      (call $3
       (f64.const 1)
      )
      (call $3
       (f64.const 2)
      )
     )
    )
   )
  )
  (drop
   (struct.new $0
    (call $13
     (local.get $2)
     (local.get $3)
    )
   )
  )
  (drop
   (if (result (ref null $0))
    (i32.eq
     (struct.get $0 0
      (struct.new $0
       (call $13
        (local.get $2)
        (local.get $3)
       )
      )
     )
     (i32.const 1)
    )
    (struct.new $0
     (i32.eqz
      (call $13
       (local.get $2)
       (local.get $3)
      )
     )
    )
    (struct.new $0
     (i32.const 0)
    )
   )
  )
  (drop
   (if (result (ref null $0))
    (i32.eq
     (struct.get $0 0
      (struct.new $0
       (call $13
        (local.get $2)
        (local.get $3)
       )
      )
     )
     (i32.const 1)
    )
    (struct.new $0
     (call $13
      (local.get $2)
      (local.get $3)
     )
    )
    (struct.new $0
     (i32.const 0)
    )
   )
  )
  (drop
   (if (result (ref null $0))
    (i32.eq
     (struct.get $0 0
      (struct.new $0
       (i32.eqz
        (call $13
         (local.get $2)
         (local.get $3)
        )
       )
      )
     )
     (i32.const 1)
    )
    (struct.new $0
     (call $13
      (local.get $2)
      (local.get $3)
     )
    )
    (struct.new $0
     (i32.const 0)
    )
   )
  )
  (drop
   (struct.new $0
    (i32.xor
     (struct.get $0 0
      (call $6)
     )
     (i32.const 1)
    )
   )
  )
  (drop
   (struct.new $0
    (i32.xor
     (struct.get $0 0
      (call $7)
     )
     (i32.const 1)
    )
   )
  )
  (drop
   (call $0
    (i64.const -5)
   )
  )
  (drop
   (struct.new $13
    (call $0
     (i64.const 1)
    )
    (call $0
     (i64.const 2)
    )
   )
  )
  (drop
   (struct.new $9
    (struct.new $9
     (ref.null none)
     (call $0
      (i64.const 2)
     )
    )
    (call $0
     (i64.const 1)
    )
   )
  )
  (drop
   (struct.new $9
    (struct.new $9
     (struct.new $9
      (struct.new $9
       (ref.null none)
       (call $0
        (i64.const 2)
       )
      )
      (call $0
       (i64.const 1)
      )
     )
     (call $0
      (i64.const 2)
     )
    )
    (call $0
     (i64.const 1)
    )
   )
  )
  (drop
   (array.new_data $28 $0
    (i32.const 0)
    (i32.const 11)
   )
  )
  (struct.get $13 0
   (struct.new $13
    (call $0
     (i64.const 2)
    )
    (call $0
     (i64.const 3)
    )
   )
  )
 )
 (func $15 (param $0 (ref null $1)) (param $1 (ref null $1)) (result (ref null $2))
  (struct.new $4
   (i32.const 0)
   (local.get $0)
   (local.get $1)
  )
 )
 (func $16 (param $0 (ref null $1)) (result (ref null $5))
  (struct.new $8
   (i32.const 0)
   (local.get $0)
  )
 )
 (func $17 (param $0 (ref null $1)) (param $1 (ref null $5)) (result (ref null $6))
  (struct.new $7
   (i32.const 0)
   (local.get $0)
   (local.get $1)
  )
 )
)

