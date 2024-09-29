open Process_tracker_lib.Activity_repo
open Process_tracker_lib.Cli

module Repo: ActivityRepoSig = ActivityRepo
module UI = Cli(Repo)

let () = 
    let _ = Repo.create_tables () in
    UI.repl ()
