let hash_k = Int64.(shift_left 1L 31)

let rec jmp_hash j n key =
  let key = Int64.(succ (mul key 2862933555777941757L)) in
  let key' = Int64.(succ (shift_right_logical key 33)) in
  let a = Int64.succ j in
  let j' = Int64.(div (mul a hash_k) key') in
  if j' >= Int64.of_int n then
    j
  else
    jmp_hash j' n key

let host ~hosts key =
  Int64.to_int (jmp_hash 0L hosts key)
