pub fn main() {
  let a = #(1, 2, 3)
  let b = #(4, 5, 6)

  let a_sum = a.0 + a.1 + a.2
  let b_sum = b.0 + b.1 + b.2

  a_sum + b_sum
}
