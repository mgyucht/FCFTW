type t = { name: string
         ; keys: Key.t option
         }

let name dict = dict.name

let get_first_key dict = dict.keys

let of_list name l =
  let rec make_keys = function
    | [] -> None
    | h :: t -> let newkey = Key.create_simple h name in
      match make_keys t with
      | None -> Some (Key.Simple newkey)
      | Some k -> let newkey' = Key.simple_insert_before newkey k' in
        newkey'
