#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let build = Pkg.build ~cmd:(fun c os files ->
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  OS.Cmd.run @@ Cmd.(Pkg.build_cmd c os %% of_list files)
   ) ()

type namespace = { name:string; src: string}

let prefix nm file =
  let open Fpath in
  dirname file // ( nm.name ^ "__" ^ basename file)

let modules nm  =
  nm.src
  |> OS.Dir.contents ~rel:false ~dotfiles:false
  >>| List.map Fpath.rem_ext
  >>| List.sort_uniq compare
  >>| List.map (prefix nm)

let pkg nm =
  let modules =
    modules nm
    >>| List.map (Pkg.lib ~exts:Exts.api) in
  let lib modules =
    (Pkg.lib ~exts:Exts.library ("lib_" ^ nm.name ))
    :: (Pkg.lib ~exts:Exts.api Fpath.( nm.src // nm.name ))
    :: (Pkg.bin ~exts:Exts.exe ("ppx/ppx_" ^ nm.name) )
    :: modules in
  modules >>| lib

let namespace = {src = "lib"; name = "tensority" }

let () =
  Pkg.describe ~build "tensority" @@ fun _c ->
  pkg namespace
