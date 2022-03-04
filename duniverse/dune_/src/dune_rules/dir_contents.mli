(** Directories contents *)
open! Dune_engine

(** This modules takes care of attaching source files of OCaml, C, Coq found in
    the source tree or generated by user rules to library, executables, tests
    and documentation stanzas to language specific collectors.

    It also helps handle [(include_subdirs ..)] as it handles the directory
    traversal. *)

open! Stdune
open Import

type t

val dir : t -> Path.Build.t

(** Files in this directory. At the moment, this doesn't include all generated
    files, just the ones generated by [rule], [ocamllex], [ocamlyacc], [menhir]
    stanzas. *)
val text_files : t -> String.Set.t

(** C/C++ sources *)
val foreign_sources : t -> Foreign_sources.t Memo.Build.t

val ocaml : t -> Ml_sources.t Memo.Build.t

(** Artifacts defined in this directory *)
val artifacts : t -> Ml_sources.Artifacts.t Memo.Build.t

(** All mld files attached to this documentation stanza *)
val mlds : t -> Dune_file.Documentation.t -> Path.Build.t list Memo.Build.t

val coq : t -> Coq_sources.t Memo.Build.t

(** Get the directory contents of the given directory. *)
val get : Super_context.t -> dir:Path.Build.t -> t Memo.Build.t

(** All directories in this group if [t] is a group root or just [t] if it is
    not part of a group. *)
val dirs : t -> t list

type triage =
  | Standalone_or_root of
      { root : t
      ; subdirs : t list  (** Sub-directories part of the group *)
      ; rules : Rules.t
      }
  | Group_part of Path.Build.t

(** In order to compute the directory contents, we need to interpret stanzas
    such as [rule] or [copy_files]. For such stanzas, computing the targets is
    very similar to interpreting the stanza and compiling it down to low-level
    rules.

    As a result, we proceed as follow: we interpret the stanza into rules and
    extract the targets of the computed rule. This function returns these rules.

    However, if the directory is part of a group, this function simply returns
    the root of the group. *)
val triage : Super_context.t -> dir:Path.Build.t -> triage Memo.Build.t

(** Add expansion that depend on OCaml artifacts/sources.

    This function live in super_context.ml or expander.ml because it would
    introduce a dependency cycle. *)
val add_sources_to_expander : Super_context.t -> Expander.t -> Expander.t
