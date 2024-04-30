pub type Token {
  KwPub
  KwFn
  Name(String)
  LParen
  RParen
  LBrace
  RBrace
  Unknown(String)
}

fn lex(source: String) -> List(Token) {
  case source {
    " " <> rest -> lex(rest)

    "" -> []

    "(" <> rest -> [LParen, ..lex(rest)]
    ")" <> rest -> [RParen, ..lex(rest)]
    "{" <> rest -> [LBrace, ..lex(rest)]
    "}" <> rest -> [RBrace, ..lex(rest)]

    "a" as c <> rest
    | "b" as c <> rest
    | "c" as c <> rest
    | "d" as c <> rest
    | "e" as c <> rest
    | "f" as c <> rest
    | "g" as c <> rest
    | "h" as c <> rest
    | "i" as c <> rest
    | "j" as c <> rest
    | "k" as c <> rest
    | "l" as c <> rest
    | "m" as c <> rest
    | "n" as c <> rest
    | "o" as c <> rest
    | "p" as c <> rest
    | "q" as c <> rest
    | "s" as c <> rest
    | "t" as c <> rest
    | "u" as c <> rest
    | "v" as c <> rest
    | "w" as c <> rest
    | "x" as c <> rest
    | "y" as c <> rest
    | "z" as c <> rest -> {
      let #(name, rest) = take_name(c, rest)

      let token = case name {
        "pub" -> KwPub
        "fn" -> KwFn
        _ -> Name(name)
      }

      [token, ..lex(rest)]
    }

    _ -> [Unknown(source)]
  }
}

fn take_name(name: String, source: String) -> #(String, String) {
  case source {
    "a" as c <> rest
    | "b" as c <> rest
    | "c" as c <> rest
    | "d" as c <> rest
    | "e" as c <> rest
    | "f" as c <> rest
    | "g" as c <> rest
    | "h" as c <> rest
    | "i" as c <> rest
    | "j" as c <> rest
    | "k" as c <> rest
    | "l" as c <> rest
    | "m" as c <> rest
    | "n" as c <> rest
    | "o" as c <> rest
    | "p" as c <> rest
    | "q" as c <> rest
    | "r" as c <> rest
    | "s" as c <> rest
    | "t" as c <> rest
    | "u" as c <> rest
    | "v" as c <> rest
    | "w" as c <> rest
    | "x" as c <> rest
    | "y" as c <> rest
    | "z" as c <> rest -> take_name(name <> c, rest)
    _ -> #(name, source)
  }
}

fn length(list: List(Token)) -> Int {
  case list {
    [] -> 0
    [_, ..rest] -> 1 + length(rest)
  }
}

pub fn main() {
  let tokens = lex("pub fn main() { }")

  length(tokens)
}
