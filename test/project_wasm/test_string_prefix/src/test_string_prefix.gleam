fn take_ab(count: Int, total: String, source: String) -> #(Int, String, String) {
  case source {
    "a" as a <> rest | "b" as a <> rest -> take_ab(count + 1, total <> a, rest)
    _ -> #(count, total, source)
  }
}

pub fn main() {
  let #(count, _, _) = take_ab(0, "", "abcdef")

  count
}
