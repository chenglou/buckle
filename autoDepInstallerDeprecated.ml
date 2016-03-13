(* let unused =
  (* TODO: reorder the warnings. e.g. the warning about paths with no empty
     spaces should come first *)
  if BatArray.length Sys.argv = 1 then print_endline "Please pass the name of a file to build."
  else (
    let fileName = Sys.argv.(1) in
    if not (Sys.file_exists fileName) then print_endline @@ fileName ^ " cannot be found."
    else
      let files = filesInAllDirs (Path cwd) in
      let clashes = sameFileNameClashes files in
      if BatList.length clashes > 0 then
        Printf.printf
          "We detected files of the same name:\n%s.\n\nUnder this build system, \
           every file in your own library needs to be unique, since they correspond to \
           the name of the module you can freely refer to anywhere in the library.\n"
          (formatClashes clashes)
      else
        let fileHasSpaces (Path p) = BatString.exists p " " in
        let filesWithSpaces = BatList.filter fileHasSpaces files in
        if BatList.length filesWithSpaces > 0 then
          Printf.printf
            "Some of your file/folders have spaces in them, this isn't allowed.\n%s"
            (filesWithSpaces |> BatList.map (fun (Path p) -> "- " ^ p) |> BatString.join "\n")
        else
          compileAll ()
          (* let (successOutput, err, exitCode) = syscall (buildCommand ~fileName) in
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
          | Unix.WSIGNALED _ | Unix.WSTOPPED _  -> () *)
  ) *)
