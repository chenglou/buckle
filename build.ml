(* need qualified imports *)
let sp = Printf.sprintf

(* tired of converting stuff wrongly *)
type filePath = FilePath of string
type moduleName = Mod of string
type libraryName = Lib of string

(* yo I just wanna read some values from terminal geez *)
(* mostly copied from http://rosettacode.org/wiki/Execute_a_system_command#OCaml
   because I can't code *)
let syscall ?env cmd =
  let (ic, oc, ec) = Unix.open_process_full cmd (Unix.environment ()) in
  let buf1 = Buffer.create 96 in
  let buf2 = Buffer.create 48 in
  (try
     while true do Buffer.add_channel buf1 ic 1 done
   with End_of_file -> ());
  (try
     while true do Buffer.add_channel buf2 ec 1 done
   with End_of_file -> ());
  let exit_status = Unix.close_process_full (ic, oc, ec) in
  (* check_exit_status exit_status; *)
  (Buffer.contents buf1,
   Buffer.contents buf2,
   exit_status)

let filesInAllDirs () =
  (* TODO: warn somewhere if file name same as project name *)
  let rec filesInAllDirs' (FilePath dir) =
    let ignoredDirs = ["node_modules"; ".git"; "_build"] in
    let thingsInCurrDir =
      BatSys.readdir dir
      |> BatArray.map (fun file -> FilePath (Filename.concat dir file))
    in
    let filesInCurrDir =
      thingsInCurrDir
      |> BatArray.filter (fun (FilePath a) ->
          not @@ BatSys.is_directory a &&
          BatString.ends_with a ".ml"
        )
    in
    let filesInSubDirs =
      thingsInCurrDir
      |> BatArray.filter (fun (FilePath a) ->
          BatSys.is_directory a &&
          (BatList.for_all
             (fun d -> Filename.basename d <> Filename.basename a)
             ignoredDirs))
      |> BatArray.map filesInAllDirs'
    in
    BatArray.append filesInCurrDir (BatArray.concat @@ BatArray.to_list filesInSubDirs)
  in
  BatArray.to_list @@ filesInAllDirs' (FilePath "./")

let deps (): (filePath * moduleName list) list =
  let filesString =
    filesInAllDirs ()
    |> BatList.map (fun (FilePath a) -> a)
    |> BatString.concat " "
  in
  let (resultInTopologicalOrder, err, exitCode) = syscall @@ "ocamldep -sort " ^ filesString in
  match exitCode with
  | Unix.WEXITED 0 ->
    let (resultModules, err, exitCode) = syscall @@ "ocamldep -modules -one-line " ^ filesString in
    (match exitCode with
     | Unix.WEXITED 0 ->
       let filesInTopologicalOrder =
         resultInTopologicalOrder
         |> BatString.trim
         |> BatString.nsplit ~by:" "
       in
       resultModules
       |> BatString.trim
       |> BatString.nsplit ~by:"\n"
       |> BatList.sort (fun info1 info2 ->
            let file1 = BatString.slice ~last:(BatString.index info1 ':') info1 in
            let file2 = BatString.slice ~last:(BatString.index info2 ':') info2 in
            let (index1, _) = BatList.findi (fun _ file -> file = file1) filesInTopologicalOrder in
            let (index2, _) = BatList.findi (fun _ file -> file = file2) filesInTopologicalOrder in
            index1 - index2)
        |> BatList.map (fun dep ->
            let (chunk1, chunk2) = BatString.split dep ~by:":" in
            (FilePath chunk1,
              chunk2
              |> BatString.trim
              |> BatString.nsplit ~by:" "
              |> BatList.map (fun x -> Mod x)))
     | _ -> Printf.printf "Something went wrong (deps inner): %s" err; [])
  | _ -> Printf.printf "Something went wrong (deps outer): %s" err; []

(* let () = BatList.iter (fun (a, b) ->
  print_string a;
  print_string "-:-";
  BatList.iter (fun a -> print_string @@ a ^ "=") b;
  print_endline ""
) (deps ()) *)

let safeRmdir dir =
  if BatSys.file_exists dir then
    ignore @@ syscall @@ "rm -r " ^ dir

let filePathToModuleName (FilePath p) =
  Mod (Filename.basename p |> Filename.chop_extension |> BatString.capitalize)

let generateModuleAlias (Lib libName) moduleNames =
  let fileContent =
    moduleNames
    |> BatList.map (fun (Mod name) ->
      sp
        "module %s = %s__%s"
        name
        (BatString.capitalize libName)
        (BatString.uncapitalize name))
    |> BatString.join "\n"
  in
  let filePath = "_build" ^ Filename.dir_sep ^ libName ^ ".ml" in
  let outChannel = open_out filePath in
  Printf.fprintf outChannel "%s\n" fileContent;
  close_out outChannel;
  let (result, err, exitCode) = syscall (
    sp "ocamlc -no-alias-deps -w -49 -c %s" filePath
  ) in
  (match exitCode with
   | Unix.WEXITED 0 -> print_endline "everything ok"
   | _ -> Printf.printf "Something went wrong (generateModuleAlias): %s" err)

let compileForEach (Lib libName) filePaths =
  filePaths |> BatList.iter (fun (FilePath p as fp) ->
    let (Mod moduleName) = filePathToModuleName fp in
    let (out, err, exitCode) = syscall (
      sp
        "ocamlfind ocamlc -linkpkg -package batteries,pcre,yojson,bettererrors -g -open %s -I _build %s -o %s -c %s"
        (* "ocamlc -open %s -I _build %s -o %s -c %s" *)
        (BatString.capitalize libName)
        ("-I ./node_modules/*/_build")
        ("_build" ^ Filename.dir_sep ^ libName ^ "__" ^ (BatString.uncapitalize moduleName) ^ ".cmo")
        p
    ) in
    (match exitCode with
    | Unix.WEXITED 0 -> print_endline @@ "Successfully compiled: " ^ p
    | _ -> Printf.printf "Something went wrong (compileForEach): %s" err)
  )

let buildCma (Lib libName) moduleNames =
  let (out, err, exitCode) = syscall (
    sp
      "ocamlfind ocamlc -linkpkg -package batteries,pcre,yojson,bettererrors -g -open %s -I _build %s -a -o %s %s %s %s"
      (* "ocamlc -open %s -I _build %s -o %s -c %s" *)
      (BatString.capitalize libName)
      ("-I ./node_modules/*/_build")
      ("_build" ^ Filename.dir_sep ^ "lib.cma")
      ("./node_modules/*/_build/lib.cma")
      ("_build" ^ Filename.dir_sep ^ libName ^ ".cmo")
      (moduleNames
        |> BatList.map (fun (Mod name) ->
          "_build" ^ Filename.dir_sep ^ libName ^ "__" ^ (BatString.uncapitalize name) ^ ".cmo")
        |> BatString.join " ")
  ) in
  (match exitCode with
  | Unix.WEXITED 0 -> print_endline "Successfully compiled cma"
  | _ -> Printf.printf "Something went wrong (compileForEach): %s" err)

let compileAll () =
  safeRmdir "_build";
  BatUnix.mkdir "_build" 0o777;
  let libraryName = Lib (Filename.basename @@ BatSys.getcwd ()) in
  let allDeps = deps () in
  let filePaths = BatList.map BatTuple.Tuple2.first allDeps in
  let moduleNames = BatList.map filePathToModuleName filePaths in
  generateModuleAlias libraryName moduleNames;
  compileForEach libraryName filePaths;
  buildCma libraryName moduleNames

let () = compileAll ()

let sameFileNameClashes files =
  files
  |> BatList.sort (fun (FilePath a) (FilePath b) -> compare (Filename.basename a) (Filename.basename b))
  |> BatList.group_consecutive (fun (FilePath a) (FilePath b) -> Filename.basename a = Filename.basename b)
  |> BatList.filter (fun block -> BatList.length block > 1)

let formatClashes clashes =
  clashes
  |> BatList.map (fun block ->
      block
      |> BatList.map (fun (FilePath x) -> "- " ^ x)
      |> BatString.concat "\n")
  |> BatString.concat "\n\n"

let buildCommand ~fileName =
  let builtName = (Filename.chop_extension fileName) ^ ".out" in
  (* ocamfind is temporary, just for bootstrapping, until we dogfood this and
     publish batteries and pcre and yojson on npm (hyeah right, we're screwed)*)
  sp {|
    ocamlfind ocamlc -linkpkg -package batteries,pcre,yojson,bettererrors -g %s -o %s
  |} fileName builtName

let promptForInstall unboundModuleName =
  let npmModuleName = BatString.lowercase unboundModuleName in
  (* get module entry from npm, if any *)
  let (output, err, exitCode) = syscall @@ "npm info --json " ^ npmModuleName in
  match exitCode with
  | Unix.WEXITED 0 ->
    let parsed = Yojson.Basic.from_string output in
    let open Yojson.Basic.Util in
    let latestVersion = parsed |> member "dist-tags" |> member "latest" |> to_string in
    Printf.printf
      "You used `%s` but it can't be found.\nDo you want to install %s %s? Yes(y)/No(n): "
      unboundModuleName
      npmModuleName
      latestVersion;
    (* we're reading a char next; flush all garbage (this is needed) *)
    flush stdout;
    (* no need to check if package.json present, just install and when the
       person does npm init it'll be persisted correctly *)
    (try
       let ch = BatIO.read BatIO.stdin in
       if ch <> 'y' then false
       else (
         let (output, err, exitCode) = syscall @@ sp "npm install %s" npmModuleName in
         match exitCode with
         | Unix.WEXITED 0 -> true
         (* TODO: what is the install success msg? *)
         | Unix.WEXITED r -> false
         | Unix.WSIGNALED _ | Unix.WSTOPPED _  -> false)
     with BatIO.No_more_input -> false)
  | Unix.WEXITED r ->
    (* error's probably npm failing to get the module from registry; ignore it *)
    false
  | Unix.WSIGNALED _ | Unix.WSTOPPED _  -> false

let () =
  (* TODO: reorder the warnings. e.g. the warning about paths with no empty
     spaces should come first *)
  if BatArray.length Sys.argv = 1 then print_endline "Please pass the name of a file to build."
  else (
    let fileName = Sys.argv.(1) in
    if not (Sys.file_exists fileName) then print_endline @@ fileName ^ " cannot be found."
    else
      let files = filesInAllDirs () in
      let clashes = sameFileNameClashes files in
      if BatList.length clashes > 0 then
        Printf.printf
          "We detected files of the same name:\n%s.\n\nUnder this build system, \
           every file in your own library needs to be unique, since they correspond to \
           the name of the module you can freely refer to anywhere in the library.\n"
          (formatClashes clashes)
      else
        let fileHasSpaces (FilePath p) = BatString.exists p " " in
        let filesWithSpaces = BatList.filter fileHasSpaces files in
        if BatList.length filesWithSpaces > 0 then
          Printf.printf
            "Some of your file/folders have spaces in them, this isn't allowed.\n%s"
            (filesWithSpaces |> BatList.map (fun (FilePath p) -> "- " ^ p) |> BatString.join "\n")
        else
          let (successOutput, err, exitCode) = syscall (buildCommand ~fileName) in
          match exitCode with
          | Unix.WEXITED 0 ->
            (* exit code of 0 can still mean there are warnings *)
            if err <> "" then
              (print_endline @@ BetterErrorsMain.parseFromString ~customErrorParsers:[] err);
            print_endline @@ BetterErrorsMain.parseFromString ~customErrorParsers:[] successOutput
          | Unix.WEXITED 2 ->
            let unboundModuleR = {|Error: Unbound module ([\w\.]*)|} in
            let unboundModuleWithHintR = {|Unbound module [\w\.]*[\s\S]Hint: Did you mean \S+\?|} in
            if Pcre.pmatch ~pat:unboundModuleWithHintR err then
              (* there's a suggestion, probably a typo, bail and don't install dep *)
              Printf.eprintf "%s\n" (BetterErrorsMain.parseFromString ~customErrorParsers:[] err)
            else (
              try
                let foundMatch = Pcre.exec ~pat:unboundModuleR err in
                let unboundModuleName = Pcre.get_substring foundMatch 1 in
                let successfulInstall = promptForInstall unboundModuleName in
                if successfulInstall then (
                  (* TODO: now what? rerun? might have other warnings and all *)
                ) else
                  Printf.eprintf "%s\n" (BetterErrorsMain.parseFromString ~customErrorParsers:[] err)
              with Not_found | Invalid_argument _ ->
                (* no module error probably. Re-print out the compile error *)
                Printf.eprintf "%s\n" (BetterErrorsMain.parseFromString ~customErrorParsers:[] err)
            )
          | Unix.WEXITED r ->
            Printf.printf "Something went wrong (top): %s %d" err r
          | Unix.WSIGNALED _ | Unix.WSTOPPED _  -> ()
  )
