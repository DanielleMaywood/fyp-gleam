pub fn main() {
  let source = "fn main() {}"

  let #(a, rest) = case source {
    "fn" <> rest -> #(1, rest)
    _ -> #(0, source)
  }

  let #(b, rest) = case rest {
    " " <> rest -> #(1, rest)
    _ -> #(0, rest)
  }

  let #(c, rest) = case rest {
    "main" <> rest -> #(1, rest)
    _ -> #(0, rest)
  }

  let #(d, rest) = case rest {
    "(" <> rest -> #(1, rest)
    _ -> #(0, rest)
  }

  let #(e, rest) = case rest {
    ")" <> rest -> #(1, rest)
    _ -> #(0, rest)
  }

  let #(f, rest) = case rest {
    " " <> rest -> #(1, rest)
    _ -> #(0, rest)
  }

  let #(g, rest) = case rest {
    "{" <> rest -> #(1, rest)
    _ -> #(0, rest)
  }

  let #(h, rest) = case rest {
    "}" <> rest -> #(1, rest)
    _ -> #(0, rest)
  }

  let i = case rest {
    "" -> 1
    _ -> 0
  }

  a + b + c + d + e + f + g + h + i
}
