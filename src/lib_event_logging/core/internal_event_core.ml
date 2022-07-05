open Tezos_stdlib

module List = struct
  include List
  include Tezos_stdlib.TzList
end

module String = struct
  include String
  include Tezos_stdlib.TzString
  module Set = Tezos_error_monad.TzLwtreslib.Set.Make (String)
end

let valid_char c =
  match c with
  | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '@' | '-' | '_' | '+' | '=' | '~' ->
      true
  | _ -> false

let check_name_exn : string -> (string -> char -> exn) -> unit =
 fun name make_exn ->
  String.iter
    (fun c -> if valid_char c then () else raise (make_exn name c))
    name ;
  ()

(* Levels are declared from the lowest to the highest so that
   polymorphic comparison can be used to check whether a message
   should be printed. *)
type level = Log_core.level =
  | Debug
  | Info
  | Notice
  | Warning
  | Error
  | Fatal

  let should_log ~level ~sink_level =
    (* Same criteria as [Log_core.log] *)
    level >= sink_level
  
module Level = struct
  type t = level

  let default = Info

  let to_lc_level t = t

  let to_string = Log_core.string_of_level

  let of_string = Log_core.level_of_string

  let encoding =
    let open Data_encoding in
    string_enum
      (List.map
         (fun l -> (to_string l, l))
         [Debug; Info; Notice; Warning; Error; Fatal])

  include Compare.Make (struct
    type nonrec t = t

    let compare = Stdlib.compare
  end)
end

module Section : sig
  type t

  include Compare.S with type t := t

  val empty : t

  val make : string list -> t

  val make_sanitized : string list -> t

  val to_lc_section : t -> Log_core.section

  val is_prefix : prefix:t -> t -> bool

  val encoding : t Data_encoding.t

  val to_string_list : t -> string list

  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool
end = struct
  type t = {path : string list; log_core_section : Log_core.section}

  include Compare.Make (struct
    type nonrec t = t

    let compare = Stdlib.compare
  end)

  let empty = {path = []; log_core_section = Log_core.Section.make ""}

  let make sl =
    List.iter
      (fun s ->
        check_name_exn s (fun name char ->
            Printf.ksprintf
              (fun s -> Invalid_argument s)
              "Internal_event.Section: invalid name %S (contains %c)"
              name
              char))
      sl ;
    {
      path = sl;
      log_core_section = Log_core.Section.make (String.concat "." sl);
    }

  let make_sanitized sl =
    List.map (String.map (fun c -> if valid_char c then c else '_')) sl |> make

  let to_string_list s = s.path

  let to_lc_section s = s.log_core_section

  let is_prefix ~prefix main =
    try
      let _ =
        List.fold_left
          (fun prev elt ->
            match prev with
            | t :: q when String.equal t elt -> q
            | _ -> raise Not_found)
          main.path
          prefix.path
      in
      true
    with Not_found -> false

  let encoding =
    let open Data_encoding in
    conv (fun {path; _} -> path) (fun l -> make l) (list string)

  let pp fmt section = Format.fprintf fmt "%s" (String.concat "." section.path)
end

let registered_sections = ref String.Set.empty

let get_registered_sections () = String.Set.to_seq !registered_sections

let register_section section =
  registered_sections :=
    String.Set.add
      (Log_core.Section.name (Section.to_lc_section section))
      !registered_sections

module type EVENT_DEFINITION = sig
  type t

  val section : Section.t option

  val name : string

  val doc : string

  val pp : short:bool -> Format.formatter -> t -> unit

  val encoding : t Data_encoding.t

  val level : t -> level
end
