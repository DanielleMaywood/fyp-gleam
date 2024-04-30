fn test_int_guard() {
  let a = case 1 {
    x if x == 1 -> 1
    _ -> 0
  }

  let b = case 1 {
    x if x != 0 -> 1
    _ -> 0
  }

  let c = case 1 {
    x if x >= 1 -> 1
    _ -> 0
  }

  let d = case 1 {
    x if x <= 2 -> 1
    _ -> 0
  }

  let e = case 1 {
    x if x > 0 -> 1
    _ -> 0
  }

  let f = case 1 {
    x if x < 2 -> 1
    _ -> 0
  }

  a + b + c + d + e + f
}

fn test_float_guard() {
  let a = case 1.0 {
    x if x == 1.0 -> 1
    _ -> 0
  }

  let b = case 1.0 {
    x if x != 0.0 -> 1
    _ -> 0
  }

  let c = case 1.0 {
    x if x >=. 1.0 -> 1
    _ -> 0
  }

  let d = case 1.0 {
    x if x <=. 2.0 -> 1
    _ -> 0
  }

  let e = case 1.0 {
    x if x >. 0.0 -> 1
    _ -> 0
  }

  let f = case 1.0 {
    x if x <. 2.0 -> 1
    _ -> 0
  }

  a + b + c + d + e + f
}

fn test_tuple_index_guard() {
  case #(1, 2, 3) {
    x if x.1 == 2 -> 1
    _ -> 0
  }
}

fn test_or_guard() {
  case 1 {
    x if x == 0 || x == 1 -> 1
    _ -> 0
  }
}

fn test_and_guard() {
  case 1 {
    x if x >= 1 && x <= 1 -> 1
    _ -> 0
  }
}

fn test_not_guard() {
  case False {
    x if !x -> 1
    _ -> 0
  }
}

type Record {
  Record(x: Int)
}

fn test_field_access_guard() {
  case Record(x: 10) {
    r if r.x == 10 -> 1
    _ -> 0
  }
}

pub fn main() {
  let a = test_int_guard()
  let b = test_float_guard()
  let c = test_tuple_index_guard()
  let d = test_or_guard()
  let e = test_and_guard()
  let f = test_not_guard()
  let g = test_field_access_guard()

  a + b + c + d + e + f + g
}
