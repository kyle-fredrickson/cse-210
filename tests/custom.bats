load harness

@test "custom-1" {
  check '2 ^ 3' '8'
}

@test "custom-2" {
  check '2 ^ 3 ^ 2' '512'
}

@test "custom-3" {
  check '3 * 2 ^ 3 ^ 2' '1536'
}

@test "custom-4" {
  check '3 * 2 ^ 3 ^ 2 + -100' '1436'
}

@test "custom-5" {
  check '3 * 2 ^ 3 ^ 2 + -100 * 2 ^ 3' '736'
}