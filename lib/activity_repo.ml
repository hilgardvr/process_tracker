type activity = 
    { id: int64
    ; activity: string
    ; start_time: int64
    ; end_time: int64 option
    }

let db = Sqlite3.db_open "process_tracker.db"

let parse_activity_row_data row = 
(*     print_endline ("row: " ^ (Int.to_string @@ Array.length row)); *)
    let id: int64 = match row.(0) with
        | Sqlite3.Data.INT i -> i
        | _ -> failwith "Expected int for id" in
    let ac: string = match row.(1) with
        | Sqlite3.Data.TEXT t -> t
        | _ -> failwith "Expected string for activity" in
    let st: int64 = match row.(2) with
        | Sqlite3.Data.INT i -> i
        | _ -> failwith "Expected int for start time" in
    let et: int64 option = match row.(3) with
        | Sqlite3.Data.INT i -> Some i
        | _ -> None in
    { id = id
    ; activity = ac
    ; start_time = st
    ; end_time = et
    }

let activity_to_string ac = 
    let et = match ac.end_time with
        | None -> "null"
        | Some e -> Int64.to_string e in
    Printf.sprintf "id: %s, activity: %s, start_time: %s, end_time: %s" (Int64.to_string ac.id) ac.activity (Int64.to_string ac.start_time) et

let log_error res = 
    match res with
    | Sqlite3.Rc.OK -> ()
    | Sqlite3.Rc.DONE -> ()
    | Sqlite3.Rc.ROW -> ()
    | v -> 
        print_endline ("Failure " ^ Sqlite3.Rc.to_string v);
        let errmsg = Sqlite3.errmsg db  in
        print_endline @@ "Error code: " ^ errmsg

let create_tables () = 
    let create_table = "create table if not exists process (
        id integer primary key autoincrement,
        activity string not null,
        start_time integer not null,
        end_time integer
        );" in
    Sqlite3.exec db create_table

let create_entry activ start_t =
    let insert =  Printf.sprintf ("insert into process(activity, start_time, end_time) values('%s', %d, null) returning id, activity, start_time, end_time;") activ start_t in
    print_endline @@ "inserting: " ^ insert;
    let stmt = Sqlite3.prepare db insert in
    let r = Sqlite3.step stmt in
    log_error r;
    let row_blob = Sqlite3.row_data stmt in
    let act = parse_activity_row_data row_blob in
    let _ = Sqlite3.step stmt in
    act

let set_end_time id =
    let update: string = Printf.sprintf ("update process set end_time = %d where id = %d") (int_of_float @@ Unix.time ()) id in
    let res = Sqlite3.exec db update in
    log_error res

let cb (row: string option array) (header: Sqlite3.header array) = 
    Array.iter (fun a -> print_endline a) header;
    Array.iter (fun e -> 
        match e with
        | None -> print_endline "no value"
        | Some e' -> print_endline e'
    ) row

let retrive_entry () =
    let select = "select * from process limit 1" in
    let stmt = Sqlite3.prepare db select in
    let _ = Sqlite3.step stmt in
    let row_data = Sqlite3.row_data stmt in
    let parsed = parse_activity_row_data row_data in
    parsed

let retrive_distinct_activities () =
    let select = "select distinct activity from process;" in
    let stmt = Sqlite3.prepare db select in
    let rec get_data stmt ac = 
        let step = Sqlite3.step stmt in
        match step with 
        | Sqlite3.Rc.ROW -> 
            let data = Sqlite3.row_data stmt in
            let parsed = Array.fold_left (fun acc a -> 
                match a with 
                | Sqlite3.Data.TEXT t -> t :: acc
                | _ -> failwith "expected text result"
            ) ac data in
            get_data stmt parsed
        | Sqlite3.Rc.DONE -> ac
        | rc -> 
            log_error rc; 
            ac in
    get_data stmt []

let get_all () =
    let select = "select * from process" in
    let stmt = Sqlite3.prepare db select in
    let step = Sqlite3.step stmt in
    match step with
        | Sqlite3.Rc.ROW -> 
            let data = Sqlite3.row_data stmt in
            let parsed = parse_activity_row_data data in
            [parsed]
        | _ -> []


let get_activity_entries activity =
    let select = Printf.sprintf "select * from process where activity = '%s';" activity in
    let stmt = Sqlite3.prepare db select in
    let rec accumulate stmt = 
        let step = Sqlite3.step stmt in
        match step with
            | Sqlite3.Rc.ROW -> 
                let data = Sqlite3.row_data stmt in
                let parsed = parse_activity_row_data data in
                parsed :: accumulate stmt
            | _ -> [] in
    accumulate stmt
