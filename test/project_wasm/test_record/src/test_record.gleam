pub type Record {
  A(x: Int, y: Int)
  B(x: Int)
}

pub fn main() {
  let a = A(x: 12, y: 24)
  let b = B(x: 48)

  let a_y = case a {
    A(_, y) -> y
    B(_) -> 0
  }

  let b_y = case b {
    A(_, y) -> y
    B(_) -> 0
  }

  a.x + b.x + a_y + b_y
}
