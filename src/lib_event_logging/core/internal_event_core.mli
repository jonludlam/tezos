
open Tezos_stdlib

(** {3 Events Definitions and Registration } *)

(** The relative importance of a particular event (compatible with
    traditional logging systems, cf. {!Log_core.level}). *)
    type level =  Log_core.level = Debug | Info | Notice | Warning | Error | Fatal

    val should_log : level:'a -> sink_level:'a -> bool

    (** Module to manipulate values of type {!level}.  *)
    module Level : sig
      (** Alias of {!level}. *)
      type t = level
    
      (** The default level is {!Info}. *)
      val default : t
    
      (** Cast the level to a value of {!Log_core.level}. *)
      val to_lc_level : t -> Log_core.level
    
      val to_string : t -> string
    
      val of_string : string -> t option
    
      val encoding : t Data_encoding.t
    
      include Compare.S with type t := t
    end
    
    (** Sections are a simple way of classifying events at the time of
        their emission. *)
    module Section : sig
      type t
    
      val empty : t
    
      val make : string list -> t

      (** Build a {!Section.t} by replacing special characters with ['_']. *)
      val make_sanitized : string list -> t
    
      (** Make the equivalent {!Lwt_log} section.  *)
      val to_lc_section : t -> Log_core.section
    
      (** [is_prefix ~prefix p] checks that [p] starts with [~prefix].  *)
      val is_prefix : prefix:t -> t -> bool
    
      val encoding : t Data_encoding.t
    
      val to_string_list : t -> string list
    
      val pp : Format.formatter -> t -> unit
    
      include Compare.S with type t := t
    end
    
    (** All the section that has been registered. Currently, sections are registered
        by the `Simple` module and the `Legacy_logging` module. *)
    val get_registered_sections : unit -> string Seq.t
    
    val register_section : Section.t -> unit
    
    (** Parameters defining an inspectable type of events. *)
    module type EVENT_DEFINITION = sig
      type t
    
      (** Defines an optional section for the event.
    
          {b Warning} [None] is only for legacy events and
         should not be used in new code.  *)
      val section : Section.t option
    
      (** Defines the identifier for the event. Names should be unique and
          are restricted to alphanumeric characters or [".@-_+=,~"].*)
      val name : string
    
      (** A display-friendly text which describes what the event means. *)
      val doc : string
    
      (* Pretty printer for log messages.
         Some sinks output a short message; some output a more detailed message; and
         some may output both. This function is called with [~short: true] when they
         want short messages, and [~short: false] when they want detailed ones.
         Short messages should contain information which is not available in the
         event encoding, or that looks nice when inlined in the message. *)
      val pp : short:bool -> Format.formatter -> t -> unit
    
      val encoding : t Data_encoding.t
    
      (** Return the preferred {!level} for a given event instance. *)
      val level : t -> level
    end
    
    val check_name_exn : string -> (string -> char -> exn) -> unit

    val registered_sections : Tezos_error_monad.TzLwtreslib.Set.Make (String).t ref