(* let () = print_endline May.asd *)

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
  let rec filesInAllDirs' dir =
    let ignoredDirs = ["node_modules"; ".git"] in
    let thingsInCurrDir =
      BatSys.readdir dir
      |> BatArray.map (fun file -> Filename.concat dir file)
    in
    let filesInCurrDir =
      thingsInCurrDir
      |> BatArray.filter (fun a ->
        not @@ BatSys.is_directory a &&
        BatString.ends_with a ".ml"
      )
    in
    let filesInSubDirs =
      thingsInCurrDir
      |> BatArray.filter (fun thing ->
          BatSys.is_directory thing &&
          (BatList.for_all
            (fun d -> Filename.basename d <> Filename.basename thing)
            ignoredDirs))
      |> BatArray.map filesInAllDirs'
    in
    BatArray.append filesInCurrDir (BatArray.concat @@ BatArray.to_list filesInSubDirs)
  in
  BatArray.to_list @@ filesInAllDirs' "./"

let sameFileNameClashes () =
  filesInAllDirs ()
  |> BatList.sort (fun a b -> compare (Filename.basename a) (Filename.basename b))
  |> BatList.group_consecutive (fun a b -> Filename.basename a = Filename.basename b)
  |> BatList.filter (fun block -> BatList.length block > 1)

let formatClashes clashes =
  clashes
  |> BatList.map (fun block ->
    block
    |> BatList.map (fun x -> "- " ^ x)
    |> BatString.concat "\n")
  |> BatString.concat "\n\n"

let buildCommand ~fileName =
  let builtName = (Filename.chop_extension fileName) ^ ".out" in
  (* ocamfind is temporary, just for bootstrapping, until we dogfood this and
     publish batteries and pcre and yojson on npm (hyeah right, we're screwed)*)
  (* TODO: set env OCAMLRUNPARAM=b *)
  Printf.sprintf {|
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
         let (output, err, exitCode) = syscall @@ Printf.sprintf "npm install %s" npmModuleName in
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
  if BatArray.length Sys.argv = 1 then print_endline "Please pass the name of a file to build."
  else
    let fileName = Sys.argv.(1) in
    if not (Sys.file_exists fileName) then print_endline @@ fileName ^ " cannot be found."
    else
    let clashes = sameFileNameClashes () in
    if BatList.length clashes > 0 then
      Printf.printf
        "We detected files of the same name:\n%s.\n\nUnder this build system, \
every file in your own library needs to be unique, since they correspond to \
the name of the module you can freely refer to anywhere in the library.\n"
        (formatClashes clashes)
    else
      let (successOutput, err, exitCode) = syscall @@ buildCommand ~fileName in
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
        Printf.printf "Something went wrong: %s %d" err r
      | Unix.WSIGNALED _ | Unix.WSTOPPED _  -> ()
