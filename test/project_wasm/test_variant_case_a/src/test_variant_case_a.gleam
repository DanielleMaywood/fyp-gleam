pub type FooBar {
  Foo
  Bar
}

pub fn main() {
  let a = case Foo {
    Foo -> 1
    Bar -> 0
  }

  let b = case Bar {
    Foo -> 0
    Bar -> 1
  }

  a + b
}
