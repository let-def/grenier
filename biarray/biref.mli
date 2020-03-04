type (+'r, -'w) t
external make : 'a -> ('a, 'a) t = "%makemutable"
external get : ('r, _) t -> 'r = "%field0"
external set : (_, 'w) t -> 'w -> unit = "%setfield0"

external ref_as_biref : 'a ref -> ('a, 'a) t = "%identity"
external biref_as_ref : ('a, 'a) t -> 'a ref = "%identity"

