let db = Sqlite3.db_open "process_tracker.db"

type activity = 
    { id: int64
    ; activity: string
    ; start_time: int64
    ; end_time: int64 
    }

let activity_to_string ac = Printf.sprintf "id: %s, activity: %s, start_time: %s, end_time: %s" (Int64.to_string
    ac.id) ac.activity (Int64.to_string ac.start_time) (Int64.to_string ac.end_time)

let create_tables db = 
    let create_table = "create table if not exists process (
        id integer primary key autoincrement,
        activity string not null,
        start_time integer not null,
        end_time integer
        );" in
    Sqlite3.exec db create_table

let create_entry db = 
    let insert = Printf.sprintf ("insert into process(activity, start_time, end_time) values('%s', %d, %d);") "test activity" 1 1 in
    print_endline insert;
    Sqlite3.exec db insert

let row_cb row index = match row.(index) with
        | Some r -> "row " ^ r
        | None -> "no row 0"


let cb (row: string option array) (header: Sqlite3.header array) = 
    Array.iter (fun a -> print_endline a) header;
    Array.iter (fun e -> 
        match e with
        | None -> print_endline "no value"
        | Some e' -> print_endline e'
    ) row

let parse_activity_row_data row = 
    print_endline ("row: " ^ (Int.to_string @@ Array.length row));
    let id: int64 = match row.(0) with
        | Sqlite3.Data.INT i -> i
        | _ -> failwith "Expected int for id" in
    let ac: string = match row.(1) with
        | Sqlite3.Data.TEXT t -> t
        | _ -> failwith "Expected string for activity" in
    let st: int64 = match row.(2) with
        | Sqlite3.Data.INT i -> i
        | _ -> failwith "Expected int for start time" in
    let et: int64 = match row.(3) with
        | Sqlite3.Data.INT i -> i
        | _ -> failwith "Expected int for end time" in
    { id = id
    ; activity = ac
    ; start_time = st
    ; end_time = et
    }

let log_error db res = 
    match res with
    | Sqlite3.Rc.OK -> print_endline "Success full db call" 
    | v -> 
        print_endline ("Failure " ^ Sqlite3.Rc.to_string v);
        let errmsg = Sqlite3.errmsg db  in
        print_endline @@ "Error code: " ^ errmsg

let retrive_entry db =
    let select = "select * from process limit 1" in
    let stmt = Sqlite3.prepare db select in
    let res = Sqlite3.step stmt in
    log_error db res;
    let row_data = Sqlite3.row_data stmt in
    let parsed = parse_activity_row_data row_data in
    parsed

let () = 
    let db = db in
    let ret = retrive_entry db in
    print_endline (activity_to_string ret);
    print_endline (activity_to_string ret);
