pub fn main() {
  let a = case "foo" {
    "foo" -> 1
    _ -> 0
  }

  let b = case "bar" {
    "foo" -> 0
    "bar" -> 1
    _ -> 0
  }

  let c = case "baz" {
    "bazbar" -> 0
    "baz" -> 1
    _ -> 0
  }

  a + b + c
}
