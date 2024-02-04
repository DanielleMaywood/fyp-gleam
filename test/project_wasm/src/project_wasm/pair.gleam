pub type Pair {
  Pair(x: Int, y: Int)
}

pub fn add(this: Pair, other: Pair) -> Pair {
  Pair(x: this.x + other.x, y: this.y + other.y)
}

pub const global = 5
