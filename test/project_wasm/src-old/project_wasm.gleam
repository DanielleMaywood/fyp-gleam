import project_wasm/pair.{Pair, global}

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

type AorB {
  A
  B
}

type XorY {
  X(Int)
  Y(Float)
}

fn return(value: Int, before: fn() -> Int) -> Int {
  before()
  value
}

pub fn main() {
  use <- return(5)

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
  let _ = []
  let _ = [1, 2, ..[]]
  let _ = #(2, 3).0
  let _ = "h"
  let _ = "hello" <> " " <> "world"
  let _ = fib(40)
  let _ = Nil

  let _ = case A, B {
    A, B -> fib(40)
    B, A -> 0
    _, _ -> 1
  }

  let _ = case "danielle" {
    "hayleigh" -> 1
    "danielle" -> 2
    _otherwise -> 0
  }

  // let _ = case "danielle" {
  //   "dan" <> rest ->
  //     case "dan" <> rest {
  //       "danielle" -> 2
  //       _otherwise -> 1
  //     }
  //   _otherwise -> 0
  // }

  // let _ = case #(2, 3) {
  //   #(2, 3) -> 1
  //   #(_, _) -> 2
  // }

  let _ = case [1, 2] {
    [1, 3] -> 1
    _ -> 2
  }

  let _ = case X(4) {
    X(x) -> x
    Y(_) -> 0
  }

  // let _ = case [1, 2, 3, 4] {
  //   [1, 2, ..rest] ->
  //     case rest {
  //       [3, 4] -> 2
  //       _ -> 1
  //     }
  //   _ -> 0
  // }

  // let _ = case #(1, [2, 3]) {
  //   #(1, [_, _] as rest) -> {
  //     case rest {
  //       [2, 3] -> 2
  //       _ -> 1
  //     }
  //   }
  //   _ -> 0
  // }

  // let lambda = fn(x) { x * 2 }
  // let _ = lambda(5)

  let _ = pair.global
  let _ = global

  // let pair = Pair(x: 2, y: 4)
  // let pair = Pair(..pair, x: 1)
  // let _ = pair

  // let captured = 5
  // let closure = fn(x) {
  //   let y = 5

  //   captured + x + y
  // }

  // let _ = closure(10)

  0
}
