type t = { name: string
         ; keys: Key.t option
         }

let name dict = dict.name

let get_first_key dict = dict.keys

let of_list name l =
  let rec make_keys = function
    | [] -> None
    | h :: t -> let newkey = Key.create h name in
      match make_keys t with
      | None -> Some newkey
      | Some k -> let newkey' = Key.insert_before k newkey name in
        Some newkey' in
  let keys = make_keys l in
  {name; keys}
