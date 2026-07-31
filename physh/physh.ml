let null = ref ()

module Set = struct
  type 'a t

  external physh_set_alloc : 'a array -> unit ref -> 'a t = "ml_physh_set_alloc"
  external physh_set_add : 'a t -> 'a -> unit ref-> unit = "ml_physh_set_add"
  external physh_set_mem : 'a t -> 'a -> unit ref-> bool = "ml_physh_set_mem"
  external physh_set_length : 'a t -> int = "ml_physh_set_length"

  let create () = physh_set_alloc [||] null
  let length = physh_set_length
  let add t x = physh_set_add t x null
  let mem t x = physh_set_mem t x null
end

module Map = struct
  type ('a,'b) t

  external physh_map_alloc : 'a array -> unit ref -> ('a, 'b) t = "ml_physh_map_alloc"
  external physh_map_add  : ('a, 'b) t -> 'a -> 'b -> unit ref -> unit = "ml_physh_map_add"
  external physh_map_find : ('a, 'b) t -> 'a -> unit ref -> 'b = "ml_physh_map_find"
  external physh_map_length : ('a, 'b) t -> int = "ml_physh_map_length"

  let create () = physh_map_alloc [||] null
  let length = physh_map_length
  let add t k v = physh_map_add t k v null
  let find t k = physh_map_find t k null
end
