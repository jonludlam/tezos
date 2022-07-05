(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Tz_log_core

module Output : sig
  type t =
    | Null
    | Stdout
    | Stderr
    | File of string
    | Syslog of Tz_log_core_unix.Lwt_log.syslog_facility

  val encoding : t Data_encoding.t

  val of_string : string -> t option

  val to_string : t -> string

  val pp : Format.formatter -> t -> unit
end

type cfg = {
  output : Output.t;
  default_level : Internal_event_core.level;
  rules : string option;
  template : Tz_log_core_lwt.Lwt_log_tz.template;
}

val default_cfg : cfg

val create_cfg :
  ?output:Output.t ->
  ?default_level:Internal_event_core.level ->
  ?rules:string ->
  ?template:Tz_log_core_lwt.Lwt_log_tz.template ->
  unit ->
  cfg

val cfg_encoding : cfg Data_encoding.t

(** Configure the event-logging sink defined in
    {!Internal_event.Lwt_log_sink} by merging the contents of [?cfg]
    (default: {!default_cfg}) and the value of the ["TEZOS_LOG"]
    environment variable. *)
val initialize : ?cfg:cfg -> unit -> unit Lwt.t
