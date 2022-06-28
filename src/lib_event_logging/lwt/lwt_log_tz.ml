open Tz_log_core.Log_core

(* +-----------------------------------------------------------------+
   | Loggers                                                         |
   +-----------------------------------------------------------------+ *)

   exception Logger_closed

   type logger = {
     mutable lg_closed : bool;
     lg_output : section -> level -> string list -> unit Lwt.t;
     lg_close : unit Lwt.t Lazy.t;
   }
   
   let close logger =
     logger.lg_closed <- true;
     Lazy.force logger.lg_close
   
   let make ~output ~close =
     {
       lg_closed = false;
       lg_output = output;
       lg_close  = Lazy.from_fun close;
     }
   
   let broadcast loggers =
     make
       ~output:(fun section level lines ->
         Lwt_list.iter_p (fun logger -> logger.lg_output section level lines) loggers)
       ~close:Lwt.return
   
   let dispatch f =
     make
       ~output:(fun section level lines -> (f section level).lg_output section level lines)
       ~close:Lwt.return
   
   (* +-----------------------------------------------------------------+
      | Templates                                                       |
      +-----------------------------------------------------------------+ *)
   
   type template = string
   
   let location_key = Lwt.new_key ()
   
   let render ~buffer ~template ~section ~level ~message =
     let file, line, column =
       match Lwt.get location_key with
       | Some loc -> loc
       | None -> ("<unknown>", -1, -1)
     in
     Buffer.add_substitute buffer
       (function
         | "message" -> message
         | "level" -> string_of_level level
         | "section" -> Section.name section
         | "loc-file" -> file
         | "loc-line" -> string_of_int line
         | "loc-column" -> string_of_int column
         | var -> Printf.ksprintf invalid_arg "Lwt_log_core.render: unknown variable %S" var)
       template
   
   (* +-----------------------------------------------------------------+
      | Predefined loggers                                              |
      +-----------------------------------------------------------------+ *)
   
   let null =
     make
       ~output:(fun _section _level _lines -> Lwt.return_unit)
       ~close:Lwt.return
   
   let default = ref null
   
   
   (* +-----------------------------------------------------------------+
      | Logging functions                                               |
      +-----------------------------------------------------------------+ *)
   
   (* knicked from stdlib/string.ml; available since 4.04.0 *)
   let split_on_char sep s =
     let r = ref [] in
     let j = ref (String.length s) in
     for i = String.length s - 1 downto 0 do
       if String.unsafe_get s i = sep then begin
         r := String.sub s (i + 1) (!j - i - 1) :: !r;
         j := i
       end
     done;
     String.sub s 0 !j :: !r
   
   let split str =
     split_on_char '\n' str
   
   let log ?exn ?(section=Section.main) ?location ?logger ~level message =
     let logger = match logger with
       | None -> !default
       | Some logger -> logger
     in
     if logger.lg_closed then
       Lwt.fail Logger_closed
     else if level >= section.Section.level then
       match exn with
       | None ->
         Lwt.with_value location_key location (fun () -> logger.lg_output section level (split message))
       | Some exn ->
         let bt = if Printexc.backtrace_status () then Printexc.get_backtrace ()
           else "" in
         let message = message ^ ": " ^ Printexc.to_string exn in
         let message =
           if String.length bt = 0 then message
           else message ^ "\nbacktrace:\n" ^ bt
         in
         Lwt.with_value location_key location (fun () -> logger.lg_output section level (split message))
     else
       Lwt.return_unit
   
   let log_f ?exn ?section ?location ?logger ~level format =
     Printf.ksprintf (log ?exn ?section ?location ?logger ~level) format
   
   let ign_log ?exn ?section ?location ?logger ~level message =
     try
       ignore (log ?exn ?section ?location ?logger ~level message)
     with _ ->
       ()
   
   let ign_log_f ?exn ?section ?location ?logger ~level format =
     Printf.ksprintf (ign_log ?exn ?section ?location ?logger ~level) format
   
   let debug ?exn ?section ?location ?logger msg = log ?exn ?section ?location ?logger ~level:Debug msg
   let debug_f ?exn ?section ?location ?logger fmt = Printf.ksprintf (debug ?exn ?section ?location ?logger) fmt
   let info ?exn ?section ?location ?logger msg = log ?exn ?section ?location ?logger ~level:Info msg
   let info_f ?exn ?section ?location ?logger fmt = Printf.ksprintf (info ?exn ?section ?location ?logger) fmt
   let notice ?exn ?section ?location ?logger msg = log ?exn ?section ?location ?logger ~level:Notice msg
   let notice_f ?exn ?section ?location ?logger fmt = Printf.ksprintf (notice ?exn ?section ?location ?logger) fmt
   let warning ?exn ?section ?location ?logger msg = log ?exn ?section ?location ?logger ~level:Warning msg
   let warning_f ?exn ?section ?location ?logger fmt = Printf.ksprintf (warning ?exn ?section ?location ?logger) fmt
   let error ?exn ?section ?location ?logger msg = log ?exn ?section ?location ?logger ~level:Error msg
   let error_f ?exn ?section ?location ?logger fmt = Printf.ksprintf (error ?exn ?section ?location ?logger) fmt
   let fatal ?exn ?section ?location ?logger msg = log ?exn ?section ?location ?logger ~level:Fatal msg
   let fatal_f ?exn ?section ?location ?logger fmt = Printf.ksprintf (fatal ?exn ?section ?location ?logger) fmt
   
   let ign_debug ?exn ?section ?location ?logger msg = ign_log ?exn ?section ?location ?logger ~level:Debug msg
   let ign_debug_f ?exn ?section ?location ?logger fmt = Printf.ksprintf (ign_debug ?exn ?section ?location ?logger) fmt
   let ign_info ?exn ?section ?location ?logger msg = ign_log ?exn ?section ?location ?logger ~level:Info msg
   let ign_info_f ?exn ?section ?location ?logger fmt = Printf.ksprintf (ign_info ?exn ?section ?location ?logger) fmt
   let ign_notice ?exn ?section ?location ?logger msg = ign_log ?exn ?section ?location ?logger ~level:Notice msg
   let ign_notice_f ?exn ?section ?location ?logger fmt = Printf.ksprintf (ign_notice ?exn ?section ?location ?logger) fmt
   let ign_warning ?exn ?section ?location ?logger msg = ign_log ?exn ?section ?location ?logger ~level:Warning msg
   let ign_warning_f ?exn ?section ?location ?logger fmt = Printf.ksprintf (ign_warning ?exn ?section ?location ?logger) fmt
   let ign_error ?exn ?section ?location ?logger msg = ign_log ?exn ?section ?location ?logger ~level:Error msg
   let ign_error_f ?exn ?section ?location ?logger fmt = Printf.ksprintf (ign_error ?exn ?section ?location ?logger) fmt
   let ign_fatal ?exn ?section ?location ?logger msg = ign_log ?exn ?section ?location ?logger ~level:Fatal msg
   let ign_fatal_f ?exn ?section ?location ?logger fmt = Printf.ksprintf (ign_fatal ?exn ?section ?location ?logger) fmt
   