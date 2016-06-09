(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let ppx_add_tag ppx exec =
     flag ["ocaml"; "compile"; ppx ] &
     S [A "-ppx"; A exec ]

let ppx_tag hook =
   match hook with
      | After_rules ->
        flag ["ocaml"; "compile"; "ppx_tensority"] &
              S [A "-ppx"; A "ppx/ppx_tensority.native"]
      | _ -> ()

let fp = Format.fprintf
let sp = Format.sprintf

type namespace = {name: string; path:string}

let path_prefix namespace s =
  let name = namespace.name ^"__"^ s in
  namespace.path / name

let odoc name =  name ^ ".odoc"

let path_prefix_doc namespace s =
  let s = odoc @@  Filename.chop_extension s in
  let name = namespace.name ^"__"^ s in
  namespace.path / name


let module_prefix namespace s =
  namespace.name ^"__"^ s

let alias_file ?(impl=false) {name;path} =
  let name = name ^ if impl then ".ml" else ".mli" in
  path / name

let mllib {name;_} = "lib_" ^ name ^ ".mllib"
let odocl {name;_} =  name ^ ".odocl"


let find_namespaces () =
  let files = Array.to_list @@ Sys.readdir "." in
  let is_namespace name = Filename.check_suffix name ".namespace" in
  let namespace name = Filename.chop_extension name in
  files |> List.filter is_namespace |> List.map namespace


let tag_namespace ({name;path} as n) =
  let files = Array.to_list @@ Sys.readdir path in
  let tag_one file =
    let tag = [sp "with_map(%s)" name] in
    if Filename.check_suffix file ".mli" then (
      tag_file (path_prefix_doc n file) tag
    )
  ; tag_file (path_prefix n file) tag  in
  List.iter tag_one files;
  tag_file (alias_file n) ["map"]

let alias lib_name module_ =
  let lib_name = (String.capitalize_ascii lib_name) in
  let alias_name = String.capitalize_ascii module_ in
  sp "@[module %s = %s__%s @]@;" alias_name lib_name module_

  let build_path path =
    let build_dir = !Options.build_dir in
    build_dir / path

let target_path dir path = build_path @@ dir / path

module C = Command

let require_dir filename =
  if Sys.( file_exists filename ) then C.Nop
  else
    let open C in
    let cmd = sp "mkdir -p %s" filename in
    Cmd ( Sh cmd )

let ( >> ) f g x = g ( f x )

let extract_modules files =
  let module S = Set.Make(struct type t=string let compare = compare end) in
  files |> List.map Filename.(basename >> chop_extension)
  |> S.of_list |> S.elements

let make_aliases ({path; name} as n)  modules =
  let cs = require_dir path in
  let target_path = alias_file n in
  let impl_path =  alias_file ~impl:true n in
  let content = List.map (alias name) modules in
  tag_file target_path ["map"];
  tag_file impl_path ["map"];
  C.(Seq [
      cs;
      Echo (content, target_path);
      Echo( content, impl_path)
    ] )

let make_module_list kind ({name; _ } as n) modules =
  let to_name file =
    file
    |> module_prefix n |> String.capitalize_ascii
    |> sp "@[%s@]@;" in
  let target = kind n in
  let target_path = build_path target in
  C.Echo (
    (  String.capitalize_ascii name ^ "\n" ) :: List.map to_name modules
  , target_path
  )

let make_mllib = make_module_list mllib
let make_odocl = make_module_list odocl

let librule namespace =
  rule
  "Directory to lib mapping"
  ~dep:"_tags"
  ~prods:[ alias_file namespace
         ; alias_file ~impl:true namespace
         ; mllib namespace; odocl namespace ]
  ( fun _env build ->
      let {name;path} = namespace in
      let files = Sys.readdir ( ".." / path ) |> Array.to_list |> extract_modules in
      let mk_aliases  =  make_aliases namespace files in
      let mk_mllib = make_mllib namespace files in
      let mk_odocl = make_odocl namespace files in
      let _intermediary =  build (
          List.map
            ( fun file -> [ path / (name  ^"__"^file )] )
            files) in
      C.Seq [ mk_aliases; mk_mllib; mk_odocl ]
  )


let prefix_ml ( {path; _ } as n ) = rule
    "Prefix .ml files"
    ~deps:[ path / "%(src).ml"]
    ~prod:( path_prefix n "%(src).ml")
    (fun env _build ->
       let src = env "%(src).ml" in
       let ori = path / src and dest = path_prefix n src in
       let tags = tags_of_pathname ori (* ++ (sp "with_map(%s)" name) *) in
       tag_file dest (Tags.elements tags);
       mv ori dest
    )

let prefix_mli ( {path; _ } as n ) = rule
    "Prefix .mli files"
    ~deps:[ path / "%(src).mli"]
    ~prod:( path_prefix n "%(src).mli")
    (fun env _build ->
       let src = env "%(src).mli" in
       let ori = path / src and dest = path_prefix n src in
       let tags = tags_of_pathname ori in
       tag_file dest (Tags.elements tags);
       mv ori dest
    )



let namespace namespace =
  let () = tag_namespace namespace in
  function
  | Before_rules ->
    flag ["ocamldep"; "map"] (C.A "-as-map")
  ; pflag ["ocamldep"] "with_map"
      (fun name -> C.(S
                        [ A"-map"; A (alias_file namespace) ;
                          A "-open"; A (String.capitalize_ascii name)  ]
                     )
      )
  ; flag ["ocaml";"compile"; "map"]
      ( C.S C.[ A "-no-alias-deps"; A "-w"; A "-49" ] )
  ; pflag ["ocaml";"compile"] "with_map"
      (fun name ->  C.S [ C.A "-no-alias-deps"; C.A "-open"
                        ; C.A (String.capitalize_ascii name) ]  )
  ; pflag ["doc";"ocaml"] "with_map"
      (fun name ->  C.S [ C.A "-open"
                        ; C.A (String.capitalize_ascii name) ]  )
  ; prefix_ml namespace
  ; prefix_mli namespace
  ; librule namespace
  | _ -> ()


let multidispatch l = dispatch @@ fun hook ->
  List.iter ( (|>) hook) l

let tensority = {name="tensority"; path="lib"}

let () =
  multidispatch [ppx_tag; namespace tensority]
