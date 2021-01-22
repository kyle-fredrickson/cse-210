load harness

@test "custom-1" {
  check 'a[0] := 5' '{a0 → 5}'
}
@test "custom-2" {
  check 'a[1 + 2] := 5' '{a3 → 5}'
}
@test "custom-3" {
  check 'i := 1 ; a[i + 2] := 5' '{a3 → 5, i → 1}'
}
@test "custom-4" {
  check 'i := 0 ; j := 1 ; a[i] := 3 ; a[j] := 4; tmp := a[i]; a[i] := a[j] ; a[j] := tmp' '{a0 → 4, a1 → 3, i → 0, j → 1, tmp → 3}'
}
@test "custom-5" {
  check 'i := 0 ; while i < 5 do { a[i] := i ; i := i + 1}' '{a0 → 0, a1 → 1, a2 → 2, a3 → 3, a4 → 4, i → 5}'
}