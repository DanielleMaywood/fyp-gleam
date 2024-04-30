pub fn main() {
  let a = case [1, 2, 3, 4] {
    [1, 2, ..rest] ->
      case rest {
        [3, 4] -> 1
        _ -> 0
      }
    _ -> 0
  }

  let b = case [1, 2, ..[3, 4]] {
    [1, 2, 3, 4, ..rest] ->
      case rest {
        [] -> 1
        _ -> 0
      }
    _ -> 0
  }

  a + b
}
