pub type Foo {
  FooWithBar(Bar)
  FooWithoutBar
}

pub type Bar {
  BarWithFoo(Foo)
  BarWithoutFoo
}

pub fn main() {
  0
}
