
open Taglib

type ft = {
	path : string;
	file : File.file_type File.file;
	tags_saved : bool;
	file_open : bool;
}

let dry_run = ref false

let fatal s =
	Printf.eprintf "FATAL: %s\n" s;
	exit 1

let string_tags = [
	tag_set_title, tag_title;
	tag_set_artist, tag_artist;
	tag_set_album, tag_album;
	tag_set_comment, tag_comment;
	tag_set_genre, tag_genre;
]

let get_file f = f.file

let map_tags fn f =
	let file = get_file f in
	List.iter (fun (set, get) -> set file (fn (try get file with _ -> ""))) string_tags;
	{ f with tags_saved = false }


(* Fix charset *)

let detect_charset f =
	let s = Buffer.create 1000 in
	Hashtbl.iter (fun _ l -> List.iter (Buffer.add_string s) l) (File.properties f);
	let s = Buffer.contents s in
	Russian.detect_charset s

let recode src t =
	let open Russian in
	Encoding.recode_string ~src:enc_utf8 ~dst:enc_latin1 t |> Encoding.recode_string ~src ~dst:enc_utf8

let fix_charset f =
	let file = get_file f in
	let enc = detect_charset file in
	match enc with
		| None ->
			if !dry_run then Printf.eprintf "fix_charset: no invalid russian charset found\n";
			f
		| Some enc ->
			if !dry_run then Printf.eprintf "fix_charset: fixing russian charset\n";
			map_tags (recode enc) f


(* Replace characters *)

let fix_chars chars f =
	if !dry_run then Printf.eprintf "fix_chars: fixing chars: '%s'\n" chars;
	let rex = Pcre.regexp ("[" ^ chars ^ "]+") in
	map_tags (Pcre.qreplace ~rex ~templ:"_") f


(* Move file *)

let mkdir =
	let cache = Hashtbl.create 101 in
	let rex = Pcre.regexp "/" in
	fun p ->
		if Hashtbl.mem cache p then
			()
		else
			match Pcre.split ~rex p with
				| [] -> ()
				| hd :: [] -> Unix.mkdir hd 0o755
				| hd :: tl ->
					let _ = List.fold_left (fun prev el ->
						let el = prev ^ "/" ^ el in
						if not (Sys.file_exists el) then
							if !dry_run then
								Printf.eprintf "mkdir %s\n" el
							else
								Unix.mkdir el 0o755;
						el
					) hd tl in
					Hashtbl.add cache p ()

let ext_of_type = function
	| `Mpeg -> "mpeg"
	| `OggVorbis -> "ogg"
	| `Flac -> "flac"
	| `OggFlac -> "ogg"
	| `Mp4 -> "mp4"
	| `Asf -> "asf"
	| `Mpc
	| `WavPack
	| `TrueAudio
	| `Speex
	| `Autodetect -> "UNK"

let get_extension f =
	try
		FilePath.get_extension f.path
	with
		| _ ->
			let file = get_file f in
			ext_of_type (File.file_type file)

let open_file path =
	let open Taglib in
	let file = File.open_file `Autodetect path in
	{ path; file; tags_saved = true; file_open = true; }

let close_file f =
	let file = get_file f in
	let f = 
		if not f.tags_saved then (
			if !dry_run then
				Printf.eprintf "Save tags to file '%s'\n" f.path
			else
				if not (File.file_save file) then
					fatal (Printf.sprintf "Cannot save tags to file %s\n" f.path);
			{ f with tags_saved = true }
		) else
			f
	in
	if f.file_open then
		File.close_file file;
	{ f with file_open = false }


let substitude_tags =
	let open Pcre in
	let counter = ref 0 in
	let l = [
		regexp "%t", tag_title;
		regexp "%b", tag_album;
		regexp "%a", tag_artist;
		regexp "%c", tag_comment;
		regexp "%g", tag_genre;
		regexp "%n", (fun f -> Printf.sprintf "%02i" (try tag_track f with _ -> incr counter; !counter));
		regexp "%y", (fun f -> Printf.sprintf "%4i" (tag_year f));
	] in
	fun f s ->
		let file = get_file f in
	  	List.fold_left (fun prev (rex, fn) -> qreplace ~rex ~templ:(try fn file with _ -> "_") prev) s l

let move_file =
	let process p f =
		let s = substitude_tags f p in
		let dir = FilePath.dirname s in
		mkdir dir;
		let s = s ^ "." ^ (get_extension f) in
		if !dry_run then
			Printf.eprintf "rename file '%s' to '%s'\n" f.path s
		else
			Unix.rename f.path s;
		close_file f
	in
	fun p -> Unix.handle_unix_error (process p)

let print_file f s =
	let s = substitude_tags f s in
	print_endline s;
	f


(* Set tags *)

let set_title f s = tag_set_title f.file s; f
let set_album f s = tag_set_album f.file s; f
let set_artist f s = tag_set_artist f.file s; f
let set_genre f s = tag_set_genre f.file s; f
let set_comment f s = tag_set_comment f.file s; f
let set_year f s = tag_set_year f.file s; f
let set_track f s = tag_set_track f.file s; f


(* Scan filename *)

let scan_name =
	let open Pcre in
	let subst_rex = regexp "(%[tbacgny])" in
	fun mask f ->
		let accepters = ref [] in
		let subst s =
			let f = match s.[1] with
				| 't' -> tag_set_title
				| 'b' -> tag_set_album
				| 'a' -> tag_set_artist
				| 'g' -> tag_set_genre
				| 'c' -> tag_set_comment
				| 'y' -> fun f s -> int_of_string s |> tag_set_year f
				| 'n' -> fun f s -> int_of_string s |> tag_set_track f
				| _ -> fatal "??"
			in
			accepters := f :: !accepters;
			"(.+?)"
		in
		let rex = substitute ~rex:subst_rex ~subst mask |> regexp in
		let accepters = List.rev !accepters in
		let filename = FilePath.basename f.path in
		let fatal () = fatal (Printf.sprintf "Filename scanner '%s' do not match with filename '%s'" mask filename) in
		try
			let r = exec ~rex filename in
			for i=1 to List.length accepters do
				try
					let s = get_substring r i in
					let fn = List.nth accepters (i-1) in
					fn f.file s;
				with
					| e -> print_endline (Printexc.to_string e); exit 1
			done;
			f
		with
			| _ -> fatal ()


(********************************************)

let get_files_list dir =
	let dirs = Hashtbl.create 101 in
	let files = ref [] in
	let rec loop p =
		if Sys.file_exists p then
			if Sys.is_directory p && not (Hashtbl.mem dirs p) then (
				Hashtbl.add dirs p ();
				let dirs = Sys.readdir p in
				Array.iter (fun el -> loop (p ^ "/" ^ el)) dirs
			) else (
				try
					files := open_file p :: !files
				with
					| _ -> ()
			)
	in
	loop dir;
	List.rev !files

(********************************************)

type filter =
	| FixCyrillic
	| FixChars of string
	| MoveFile of string
	| Title of string
	| Album of string
	| Artist of string
	| Genre of string
	| Comment of string
	| Year of int
	| Track of int
	| Print of string
	| ScanName of string

let usage () = print_endline "

tagedit /path/to/files filter1 filter2 ...

Filters:

	fix_cyrillic
	fix_chars=/()%...

	move_file='/new/path/%a/%y - %b/%n - %t'

	scan_filename='%a - %t.mp3'

	title=...
	album=...
	artist=...
	year=...
	track=...
	genre=...
	comment=...

	dry_run
	print=%a/%b...
";
	exit 1

let parse_args =
	let rex_k = Pcre.regexp "([^=]+)" in
	let rex_v = Pcre.regexp "[^=]+=(.*)" in
	let error arg = fatal (Printf.sprintf "Invalid command line argument: '%s'" arg) in
	let get_v arg =
		let r = Pcre.exec ~rex:rex_v arg in
		try
			Pcre.get_substring r 1
		with
			| _ -> error arg
	in
	let get_i arg = try int_of_string (get_v arg) with _ -> error arg in
	let parse_arg arg =
		let r = Pcre.exec ~rex:rex_k arg in
		try
			let key = Pcre.get_substring r 1 in
			match key with
				| "fix_cyrillic" -> [FixCyrillic]
				| "fix_chars" -> [FixChars (get_v arg)]
				| "move_file" -> [MoveFile (get_v arg)]
				| "title" -> [Title (get_v arg)]
				| "album" -> [Album (get_v arg)]
				| "artist" -> [Artist (get_v arg)]
				| "genre" -> [Genre (get_v arg)]
				| "comment" -> [Comment (get_v arg)]
				| "year" -> [Year (get_i arg)]
				| "track" -> [Track (get_i arg)]
				| "print" -> [Print (get_v arg)]
				| "dry_run" -> dry_run := true; []
				| "scan_name" -> [ScanName (get_v arg)]
				| _ -> error arg
		with
			| _ -> error arg
	in
	fun () ->
		match Array.to_list Sys.argv with
			| [] | [_] | [_; _] -> usage ()
			| _ :: path :: lst ->
				let filters = List.map parse_arg lst in
				let filters = List.concat filters in
				path, filters

let process_filter f = function
	| FixCyrillic -> fix_charset f
	| FixChars chars -> fix_chars chars f
	| MoveFile newpath -> move_file newpath f
	| Title s -> set_title f s
	| Album s -> set_album f s
	| Artist s -> set_artist f s
	| Genre s -> set_genre f s
	| Comment s -> set_comment f s
	| Year s -> set_year f s
	| Track s -> set_track f s
	| Print s -> print_file f s
	| ScanName mask -> scan_name mask f

let string_of_filter = function
	| FixCyrillic -> "fix_cyrillic"
	| FixChars _ -> "fix_chars"
	| MoveFile _ -> "move_file"
	| Title _ -> "title"
	| Album _ -> "album"
	| Artist _ -> "artist"
	| Genre _ -> "genre"
	| Comment _ -> "comment"
	| Year _ -> "year"
	| Track _ -> "track"
	| Print _ -> "print"
	| ScanName _ -> "scan_name"

let string_of_filters l = List.map string_of_filter l |> String.concat " "

let process_file filters f =
	let f = List.fold_left process_filter f filters in
	let _ = close_file f in
	()

let () =
	let (path, filters) = parse_args () in
	if !dry_run then Printf.eprintf "Filters applied: %s\n" (string_of_filters filters);
	if List.length filters = 0 then
		fatal "No filters specified";
	let files = get_files_list path in
	Printf.printf "Found %i files\n" (List.length files);
	List.iter (process_file filters) files

