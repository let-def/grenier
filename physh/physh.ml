let null = ref ()

module Set = struct
  type 'a t

  external phys_set_alloc : 'a array -> unit ref -> 'a t = "ml_phys_set_alloc"
  external phys_set_add : 'a t -> 'a -> unit ref-> unit = "ml_phys_set_add"
  external phys_set_mem : 'a t -> 'a -> unit ref-> bool = "ml_phys_set_mem"
  external phys_set_length : 'a t -> int = "ml_phys_set_length"

  let create () = phys_set_alloc [||] null
  let length = phys_set_length
  let add t x = phys_set_add t x null
  let mem t x = phys_set_mem t x null
end

module Map = struct
  type ('a,'b) t

  external phys_map_alloc : 'a array -> unit ref -> ('a, 'b) t = "ml_phys_map_alloc"
  external phys_map_add  : ('a, 'b) t -> 'a -> 'b -> unit ref -> unit = "ml_phys_map_add"
  external phys_map_find : ('a, 'b) t -> 'a -> unit ref -> 'b = "ml_phys_map_find"
  external phys_map_length : ('a, 'b) t -> int = "ml_phys_map_length"

  let create () = phys_map_alloc [||] null
  let length = phys_map_length
  let add t k v = phys_map_add t k v null
  let find t k = phys_map_find t k null
end
