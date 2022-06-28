(* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2002 Shawn Wagner <raevnos@pennmush.org>
 *               2009 Jérémie Dimino <jeremie@dimino.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

(* This code is an adaptation of [syslog-ocaml] *)

(* Errors happening in this module are always logged to [stderr]: *)
let log_intern fmt =
  Printf.eprintf ("Lwt_log: " ^^ fmt ^^ "\n%!")

(* +-----------------------------------------------------------------+
   | Log levels                                                      |
   +-----------------------------------------------------------------+ *)

type level =
  | Debug
  | Info
  | Notice
  | Warning
  | Error
  | Fatal

let string_of_level = function
  | Debug -> "debug"
  | Info -> "info"
  | Notice -> "notice"
  | Warning -> "warning"
  | Error -> "error"
  | Fatal -> "fatal"

let level_of_string str =
  let str = String.lowercase_ascii str in
  match str with
  | "debug" -> Some Debug
  | "info" -> Some Info
  | "notice" -> Some Notice
  | "warning" -> Some Warning
  | "error" -> Some Error
  | "fatal" -> Some Fatal
  | _ -> None

(* +-----------------------------------------------------------------+
   | Patterns and rules                                              |
   +-----------------------------------------------------------------+ *)

(* A pattern is represented by a list of literals:

   For example ["foo*bar*"] is represented by ["foo"; "bar"; ""]. *)

let sub_equal str ofs patt =
  let str_len = String.length str and patt_len = String.length patt in
  let rec loop ofs ofs_patt =
    ofs_patt = patt_len || (str.[ofs] = patt.[ofs_patt] && loop (ofs + 1) (ofs_patt + 1))
  in
  ofs + patt_len <= str_len && loop ofs 0

let pattern_match pattern string =
  let length = String.length string in
  let rec loop offset pattern =
    if offset = length then
      pattern = [] || pattern = [""]
    else
      match pattern with
      | [] ->
        false
      | literal :: pattern ->
        let literal_length = String.length literal in
        let max_offset = length - literal_length in
        let rec search offset =
          offset <= max_offset
          && ((sub_equal string offset literal && loop (offset + literal_length) pattern)
              || search (offset + 1))
        in
        search offset
  in
  match pattern with
  | [] ->
    string = ""
  | literal :: pattern ->
    sub_equal string 0 literal && loop (String.length literal) pattern

let split pattern =
  let len = String.length pattern in
  let rec loop ofs =
    if ofs = len then
      [""]
    else
      match try Some(String.index_from pattern ofs '*') with Not_found -> None with
      | Some ofs' ->
        String.sub pattern ofs (ofs' - ofs) :: loop (ofs' + 1)
      | None ->
        [String.sub pattern ofs (len - ofs)]
  in
  loop 0

let rules = ref []

let load_rules' str fail_on_error =
  let rec loop = function
    | [] -> []
    | (pattern, level_str) :: rest ->
      let pattern = split pattern in
      let level = level_of_string level_str in
      match level with
      | Some level -> (pattern, level) :: loop rest
      | None ->
        if fail_on_error then raise (Failure "Invalid log rules")
        else log_intern "invalid log level (%s)" level_str; loop rest
  in
  match Log_rules.rules (Lexing.from_string str) with
  | None ->
    if fail_on_error then raise (Failure "Invalid log rules")
    else Printf.eprintf "Invalid log rules\n%!"
  | Some l -> rules := loop l


let _ =
  match try Some(Sys.getenv "LWT_LOG") with Not_found -> None with
  | Some str -> load_rules' str false
  | None -> ()

(* +-----------------------------------------------------------------+
   | Sections                                                        |
   +-----------------------------------------------------------------+ *)

module Section =
struct
  type t = {
    name : string;
    mutable level : level;
    mutable modified : bool;
  }

  type section = t

  module Sections = Weak.Make(struct
      type t = section
      let equal a b = a.name = b.name
      let hash s = Hashtbl.hash s.name
    end)

  let sections = Sections.create 32

  let find_level name =
    let rec loop = function
      | [] ->
        Notice
      | (pattern, level) :: rest ->
        if pattern_match pattern name then
          level
        else
          loop rest
    in
    loop !rules

  let recompute_levels () =
    Sections.iter
      (fun section ->
         if not section.modified then
           section.level <- find_level section.name)
      sections

  let make name =
    let section = { name = name; level = Notice; modified = false } in
    try
      Sections.find sections section
    with Not_found ->
      section.level <- find_level name;
      Sections.add sections section;
      section

  let name section = section.name

  let main = make "main"

  let level section = section.level

  let set_level section level =
    section.level <- level;
    section.modified <- true

  let reset_level section =
    if section.modified then begin
      section.modified <- false;
      section.level <- find_level section.name
    end
end

type section = Section.t

let load_rules ?(fail_on_error=false) str =
  load_rules' str fail_on_error;
  Section.recompute_levels ()

let add_rule pattern level =
  rules := (split pattern, level) :: !rules;
  Section.recompute_levels ()

let append_rule pattern level =
  rules := !rules @ [(split pattern, level)];
  Section.recompute_levels ()

let reset_rules () =
  rules := [];
  Section.recompute_levels ()
