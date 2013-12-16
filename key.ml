(** Implementation of a triply-linked list, an extension of a doubly linked
    list. A node in a triply linked list serves one of three purposes:

    {ul {- [DictEnd] - the end of the list}
    {- [Simple] - a normal linked list node with a next and prev pointer}
    {- [Bridge] - a special linked list node. A [Bridge] node represents a key
    in common between several lists, and the two lists are maintained as two
    fields within the node.}}

    For now, the types of [dict_id_t] and [val_t] have been hardcoded into
    the system, but later they could be modularized using a functor.
 *)

(** The type of the identifier for the dictionary. *)
type dict_id_t = string

(** The type of the value of nodes in the list. *)
type val_t = string

module StringMap = Map.Make(struct
                     type t = string
                     let compare = compare
                   end)

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
                }

(** [simple_t] is the type of a [Simple] node. It has a value of [val_t]
     and a link of [link_t]. *)
and  simple_t = { svalue: val_t (** The value of the simple node. *)
                ; link: link_t (** The links to nodes before and after
                this node. *)
                ; dict: dict_id_t (** The dictionary id for this node *)
                }

(** [bridge_t] is the type of a [Bridge] node. Unlike a [Simple] node, a
    [Bridge] node uses a [StringMap] to keep track of which dictionaries
    this node is included in. *)
and  bridge_t = { bvalue: val_t (** The value of the [Bridge] node. *)
                ; links: link_t StringMap.t (** The set of dictionaries which
                this node is linked to. *)
                }

(** Creates a simple key containing [value] for dictionary [did]. *)
let create_simple value did =
  let link = {next = DictEnd; prev = DictEnd} in
  Simple {svalue = value; link; dict = did}

(** Tests whether the input node belongs to [dict]. *)
let belongs_to dict = function
  | DictEnd -> true
  | Simple {dict = d; _} -> d = dict
  | Bridge {links; _} -> StringMap.mem dict links

(** An exception that is thrown when trying to get the link of a
    [DictEnd] node. *)
exception No_link

(** Returns the link for the node for dictionary [dict], raising [No_link]
    if the node is a [DictEnd]. *)
let get_link dict = function
  | DictEnd -> raise No_link
  | Simple {link; _} -> link
  | Bridge {links; _} ->
    if StringMap.mem dict links then StringMap.find dict links
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
let rec create_bridge k1 k2 = 

  let update_next dict bridge_key = function
    | DictEnd -> ()
    | Simple {link; _} -> link.next <- bridge_key
    | Bridge {links; _} as key ->
      let link = get_link dict key in
      link.next <- bridge_key in
  let update_prev dict bridge_key = function
    | DictEnd -> ()
    | Simple {link; _} -> link.prev <- bridge_key
    | Bridge {links; _} as key ->
      let link = get_link dict key in
      link.prev <- bridge_key in
  let update_link_set ll b = 
    StringMap.mapi (fun d l ->
      update_next d b l.prev;
      update_prev d b l.next) ll in

  match k1 with
  | DictEnd -> failwith "can't create a bridge with DictEnd"

  | Simple k1 -> (match k2 with
    | DictEnd -> failwith "can't create a bridge with DictEnd"
    | Simple k2 ->
      let bvalue = k1.svalue in
      let links =    StringMap.singleton k1.dict k1.link
                  |> StringMap.add k2.dict k2.link in
      let bridge_key = Bridge {bvalue; links} in
      update_link_set links bridge_key;
      bridge_key
    | Bridge {bvalue; links} ->
      let newlinks = StringMap.add k1.dict k1.link links in
      let bridge_key = Bridge {bvalue; links = newlinks} in
      update_link_set newlinks bridge_key;
      bridge_key)

  | Bridge b1 -> match k2 with
    | DictEnd -> failwith "can't create a bridge with DictEnd"
    | Simple _ -> create_bridge k2 k1
    | Bridge {bvalue; links} ->
      assert(bvalue = b1.bvalue);
      let newlinks = StringMap.merge (fun k l1 l2 -> match (l1, l2) with
        | (Some x, None) -> Some x
        | (None, Some x) -> Some x
        | (Some x, Some y) -> Some x
        | (None, None) -> None) b1.links links in
      let bridge_key = Bridge {bvalue; links = newlinks} in
      update_link_set newlinks bridge_key;
      bridge_key

(** Converts a triply-linked list into a regular list starting with node
    [key] and using the [dict] link in any [Bridge] nodes. *)
let rec to_list key dict = match key with
  | DictEnd -> []
  | Simple {svalue; link} -> svalue :: to_list link.next dict
  | Bridge {bvalue; links} ->
      let link = get_link dict key in
    bvalue :: to_list link.next dict

(** Returns the key before [key] in [did]. *)
let prev_key key did = match key with
  | DictEnd -> failwith "no key before DictEnd"
  | Simple {link; dict; _} ->
      if dict = did then link.prev
      else failwith "did must match the dictionary id"
  | Bridge {links; _} ->
      let link = get_link did key in
      link.prev

(** Returns the key after [key] in [did]. *)
let next_key key did = match key with
  | DictEnd -> failwith "no key after DictEnd"
  | Simple {link; dict; _} ->
      if dict = did then link.next
      else failwith "did must match the dictionary id"
  | Bridge {links; _} ->
      let link = get_link did key in
      link.next

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
