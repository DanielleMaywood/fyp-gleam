type FooBar {
  Foo(Int)
  Bar(Float)
}

pub fn main() {
  let _ = case Foo(2) {
    Foo(x) -> x
    Bar(_) -> 0
  }
}
