(** Implementation of a triply-linked list, an extension of a doubly linked
    list. A node in a triply linked list serves one of three purposes:

    {ul {- [DictEnd] - the end of the list}
    {- [Simple] - a normal linked list node with a next and prev pointer}
    {- [Bridge] - a special linked list node. A [Bridge] node represents a key
    in common between two lists, and the two lists are maintained as two
    fields within the node.}}

    For now, the types of [dict_id_t] and [val_t] have been hardcoded into
    the system, but later they could be modularized using a functor.
 *)

(** The type of the identifier for the dictionary. *)
type dict_id_t = string

(** The type of the value of nodes in the list. *)
type val_t = string

(** The basic key type. *)
type t        = DictEnd (** Indicates the end of the list. *)
              | Simple of simple_t (** Indicates a simple node in the
              list. *)
              | Bridge of bridge_t (** Indicates a bridge node between two
              lists. *)

(** [link_t] represents a link to keys before and after the current item.
    It is used in one copy in the [Simple] node and in two copies in the
    [Bridge] node. *)
and  link_t   = { mutable next: t (** The next key in the list *)
                ; mutable prev: t (** The previous key in the list *)
                ; dict: dict_id_t (** The dictionary id for this list *)
                }

(** [simple_t] is the type of a [Simple] node. It has a value of [val_t]
     and a link of [link_t]. *)
and  simple_t = { svalue: val_t (** The value of the simple node. *)
                ; link: link_t (** The links to nodes before and after
                this node. *)
                }

(** [bridge_t] is the type of a [Bridge] node. it is identical to a
    [Simple] node except for the fact that it has two links rather than
    just one. *)
and  bridge_t = { bvalue: val_t
                ; d1: link_t
                ; d2: link_t
                }

(** Creates a simple key containing [value] for dictionary [did]. *)
let create_simple value did =
  let link = {next = DictEnd; prev = DictEnd; dict = did} in
  {svalue = value; link}

(** Tests whether the input node belongs to [dict]. *)
let belongs_to dict = function
  | DictEnd -> true
  | Simple {link; _} -> link.dict = dict
  | Bridge {d1; d2; _} -> d1.dict = dict || d2.dict = dict

(** An exception that is thrown when trying to get the link of a
    [DictEnd] node. *)
exception No_link

(** Returns the link for the node for dictionary [dict], raising [No_link]
    if the node is a [DictEnd]. *)
let get_link dict = function
  | DictEnd -> raise No_link
  | Simple {link; _} -> link
  | Bridge {d1; d2; _} ->
    if d1.dict = dict then d1
    else if d2.dict = dict then d2
    else failwith "dict must match one of the dictionary ids"

(** Inserts [ins] immediately after [key] in dictionary [dict], 
    returning the new [ins] node. *)
let insert_after key ins dict =
  assert(belongs_to dict key && belongs_to dict ins);
  let inslink = get_link dict ins in
  let prevlink = get_link dict key in
  inslink.prev <- key;
  inslink.next <- prevlink.next;
  prevlink.next <- ins;
  (try 
    (let nextlink = get_link dict inslink.next in
    nextlink.prev <- ins)
  with No_link -> ());
  ins

(** Inserts [ins] immediately before [key] in dictionary [dict], 
    returning the new [ins] node. *)
let insert_before key ins dict =
  assert(belongs_to dict key && belongs_to dict ins);
  let inslink = get_link dict ins in
  let nextlink = get_link dict key in
  inslink.next <- key;
  inslink.prev <- nextlink.prev;
  nextlink.prev <- ins;
  (try 
    (let prevlink = get_link dict inslink.prev in
    prevlink.next <- ins)
  with No_link -> ());
  ins

(** Takes two keys [k1] and [k2] and forms a bridge between them. *)
let create_bridge k1 k2 =
  assert(k1.svalue = k2.svalue);
  let bvalue = k1.svalue in
  let bridge_key = {bvalue; d1 = k1.link; d2 = k2.link} in
  let update_next dict = function
    | DictEnd -> ()
    | Simple {link; _} -> link.next <- Bridge bridge_key
    | Bridge {d1; d2; _} ->
      if d1.dict = dict then d1.next <- Bridge bridge_key
      else if d2.dict = dict then d2.next <- Bridge bridge_key
      else failwith "dict must match one of the dictionary ids" in
  let update_prev dict = function
    | DictEnd -> ()
    | Simple {link; _} -> link.prev <- Bridge bridge_key
    | Bridge {d1; d2; _} ->
      if d1.dict = dict then d1.prev <- Bridge bridge_key
      else if d2.dict = dict then d2.prev <- Bridge bridge_key
      else failwith "dict must match one of the dictionary ids" in
  let dict1 = bridge_key.d1.dict in
  let dict2 = bridge_key.d2.dict in
  update_next dict1 bridge_key.d1.prev;
  update_prev dict1 bridge_key.d1.next;
  update_next dict2 bridge_key.d2.prev;
  update_prev dict2 bridge_key.d2.next;
  bridge_key

(** Converts a triply-linked list into a regular list starting with node
    [key] and using the [dict] link in any [Bridge] nodes. *)
let rec to_list key dict = match key with
  | DictEnd -> []
  | Simple {svalue; link} -> svalue :: to_list link.next dict
  | Bridge {bvalue; d1; d2} -> let link = get_link dict key in
    bvalue :: to_list link.next dict

(** Returns the key before [key] in [did]. *)
let prev_key key did = match key with
  | DictEnd -> failwith "no key before DictEnd"
  | Simple {link; _} ->
      if link.dict = did then link.prev
      else failwith "did must match the dictionary id"
  | Bridge {d1; d2; _} ->
    if d1.dict = did then d1.prev
    else if d2.dict = did then d2.prev
    else failwith "did must match one of the dictionary ids"

(** Returns the key after [key] in [did]. *)
let next_key key did = match key with
  | DictEnd -> failwith "no key after DictEnd"
  | Simple {link; _} ->
      if link.dict = did then link.next
      else failwith "did must match the dictionary id"
  | Bridge {d1; d2; _} ->
    if d1.dict = did then d1.next
    else if d2.dict = did then d2.next
    else failwith "did must match one of the dictionary ids"

(** Returns true if the node is a [Bridge]. *)
let is_bridge = function
  | DictEnd  -> false
  | Simple _ -> false
  | Bridge _ -> true

(** Returns the value of a node, failing on DictEnd. *)
let value = function
  | DictEnd -> failwith "DictEnd has no value"
  | Simple {svalue; _} -> svalue
  | Bridge {bvalue; _} -> bvalue
