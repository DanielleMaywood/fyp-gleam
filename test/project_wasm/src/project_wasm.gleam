import project_wasm/pair.{Pair}

type Foo {
  Foo(x: Int)
}

type Bar {
  Bar(x: Int, foo: Foo)
}

fn do_fib(n: Int, a: Int, b: Int) {
  case n {
    0 -> a
    1 -> b
    _ -> do_fib(n - 1, b, a + b)
  }
}

fn fib(n: Int) {
  do_fib(n, 0, 1)
}

pub fn main() {
  let pair =
    Pair(2, 8)
    |> pair.add(Pair(4, 8))

  let bar_a = Bar(x: 4, foo: Foo(x: 6))
  let bar_b = Bar(x: 4, foo: Foo(x: 6))

  let _ = pair.x + pair.y
  let _ = 4.5 +. 1.25
  let _ = True
  let _ = False
  let _ = 2 > 3
  let _ = 2.0 >. 3.0
  let _ = 1 != 3
  let _ = True == True
  let _ = True == False
  let _ = True != True
  let _ = True != False
  let _ = 1.0 == 1.0
  let _ = 1.0 == 2.0
  let _ = 1.0 != 1.0
  let _ = 1.0 != 2.0
  let _ = bar_a == bar_b
  let _ = bar_a == bar_b && bar_a != bar_b
  let _ = bar_a == bar_b && bar_a == bar_b
  let _ = bar_a != bar_b && bar_a == bar_b
  let _ = !True
  let _ = !False
  let _ = -5
  let _ = #(1, 2)
  let _ = [1, 2]
  let _ = [1, 2, ..[1, 2]]
  let _ = #(2, 3).0
  let _ = "h"
  let _ = "hello" <> " " <> "world"

  fib(40)
}
