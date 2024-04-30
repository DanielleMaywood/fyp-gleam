pub type FooBar {
  Foo(Int)
  Bar(Int)
}

pub fn main() {
  let a = case Foo(10) {
    Foo(x) -> x
    Bar(_) -> 0
  }

  let b = case Bar(20) {
    Foo(_) -> 0
    Bar(x) -> x
  }

  a + b
}
