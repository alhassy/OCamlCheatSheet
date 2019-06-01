<h1> OCamlCheatSheet </h1>

Quick reference for the OCaml language.

**The listing sheet, as PDF, can be found
[here](<https://github.com/alhassy/OCamlCheatSheet/blob/master/CheatSheet.pdf>)**,
while below is an unruly html rendition.

This reference sheet is built around the system
<https://github.com/alhassy/CheatSheet>.


# Table of Contents

1.  [Emacs Setup](#org4246da3)











The OCaml toplevel, version 4.07.1

This' a strict language;
it is strongly typed where types are inferred.

Only when interacting with the top-level interpreter,
commands must be terminated by `;;`.
OCaml uses `;` as an expression *separator* &#x2014;not a terminator!


<a id="org4246da3"></a>

# Emacs Setup

    (async-shell-command "brew install ocaml")
    (use-package tuareg) ;; Emacs’ OCaml mode

    ;; The OCaml package manager: https://opam.ocaml.org/
    (async-shell-command "brew install opam") ;; version 2.0.4

    ;; Feature-rich full replacement to OCaml's standard library
    (async-shell-command "time opam install base stdio")
    ;; This may appear to ‘hang’, but that's because it took me 13 minutes.

Ensuring libraries are loaded in toplevel ocaml sessions; the following is my
`~/.ocamlinit`.

    (* Added by OPAM. *)
      try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
      with Not_found -> ();;

    #use "topfind";;
    #require "base";;
    #require "stdio";;

Let's obtain `ocp-indent` &#x2014;OCaml's indentation tool that indents the same even if
co-workers use a different editor&#x2014; and [merlin](https://github.com/ocaml/merlin/wiki/emacs-from-scratch) which provides interactive feedback
including context-aware completion and jumping to definitions.

    (async-shell-command "time opam install ocp-indent merlin") ;; real 1m33.636s
    (use-package merlin)
    (add-hook 'tuareg-mode-hook #'merlin-mode)

    (with-eval-after-load 'merlin
      (setq merlin-command 'opam))

Now entering, say, `List.` brings a pop-up compleition menu for the contents of the
list module.

In org-src blocks, you need to enter the ocaml mode, via C-c '.
