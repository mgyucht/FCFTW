(** Implementation of a triply-linked list, an extension of a doubly linked
    list. A node in a triply linked list serves one of three purposes:

    {ul {- [DictEnd] - the end of the list}
    {- [Bridge] - a linked list node. A [Bridge] node represents a key in
    common between one or several lists, and the lists are maintained as
    a map from dictionary id to link.}}

    For now, the types of [dict_id_t] and [val_t] have been hardcoded into
    the system, but later they could be modularized using a functor.
 *)

(** The type of the identifier for the dictionary. *)
type dict_id_t = string

(** The type of the value of nodes in the list. *)
type val_t = string

(** Internal module to maintain owner set. *)
module StringMap = Map.Make(struct
                     type t = dict_id_t
                     let compare = compare
                   end)

(** Internal module to maintain link map. *)
module StringSet = Set.Make(struct
                     type t = dict_id_t
                     let compare = compare
                   end)

(** The basic key type. *)
type t        = DictEnd (** Indicates the end of the list. *)
              | Bridge of bridge_t (** Indicates a bridge node between
              one or several lists. *)

(** [link_t] represents a link to keys before and after the current item.
    It is used in the [Bridge] node inside a map. *)
and  link_t   = { mutable next: t (** The next key in the list *)
                ; mutable prev: t (** The previous key in the list *)
                }

(** [bridge_t] is the type of a [Bridge] node. A [Bridge] node uses a
    [StringMap] to keep track of which dictionaries this node is included
    in. *)
and  bridge_t = { bvalue: val_t (** The value of the [Bridge] node. *)
                ; owners: StringSet.t (** The set of owners of the value
                     in this node. *)
                ; mutable links: link_t StringMap.t (** The set of
                dictionaries which this node is linked to. *)
                }

(** Creates an empty link. *)
let empty_link () = {next = DictEnd; prev = DictEnd}

(** Creates a simple key containing [value] for dictionary [did]. *)
let create value did =
  let link = {next = DictEnd; prev = DictEnd} in
  let links = StringMap.singleton did link in
  let owners = StringSet.singleton did in
  Bridge {bvalue = value; owners; links}

(** Tests whether the input node belongs to [dict]. *)
let belongs_to dict = function
  | DictEnd -> true
  | Bridge {owners; _} -> StringSet.mem dict owners

(** Adds a link to [did] in node [key]. *)
let add_link key did = match key with
  | DictEnd -> failwith "can't add a key to a DictEnd"
  | Bridge b ->
    let link = {next = DictEnd; prev = DictEnd} in
    b.links <- StringMap.add did link b.links

(** An exception that is thrown when trying to get the link of a
    [DictEnd] node. *)
exception No_link

(** Returns the link for the node for dictionary [dict], raising [No_link]
    if the node is a [DictEnd]. If the node doesn't contain a link for
    [dict], then a link is added to the node and the new link is returned. *)
let rec get_link dict key = match key with
  | DictEnd -> raise No_link
  | Bridge {links; _} as b ->
    if StringMap.mem dict links then StringMap.find dict links
    else (add_link b dict;
    get_link dict b)

(** Inserts [ins] immediately after [key] in dictionary [dict], 
    returning the new [ins] node. *)
let insert_after key ins dict =
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

(** Converts a triply-linked list into a regular list starting with node
    [key] and using the [dict] link in any [Bridge] nodes. *)
let rec to_list key dict = match key with
  | DictEnd -> []
  | Bridge {bvalue; links; _} ->
    let link = get_link dict key in
    bvalue :: to_list link.next dict

(** Returns the key before [key] in [did]. *)
let prev_key key did = match key with
  | DictEnd -> raise No_link
  | Bridge {links; _} ->
    let link = get_link did key in
    link.prev

(** Returns the key after [key] in [did]. *)
let next_key key did = match key with
  | DictEnd -> raise No_link
  | Bridge {links; _} ->
    let link = get_link did key in
    link.next

(** Returns true if the node is a [Bridge]. *)
let is_bridge = function
  | DictEnd  -> false
  | Bridge {links; _} ->
    StringMap.cardinal links > 1

(** Returns the value of a node, failing on DictEnd. *)
let value = function
  | DictEnd -> failwith "DictEnd has no value"
  | Bridge {bvalue; _} -> bvalue
