
open Taglib

type ft = {
	path : string;
	file : File.file_type File.file;
	tags_saved : bool;
}

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
		| None -> f
		| Some enc -> map_tags (recode enc) f


(* Replace characters *)

let fix_chars chars f =
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
	{ path; file; tags_saved = true }

let close_file f =
	let file = get_file f in
	if not f.tags_saved then
		if not (File.file_save file) then
			fatal (Printf.sprintf "Cannot save tags to file %s\n" f.path);
	File.close_file file

let move_file =
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
	let process p f =
		let file = get_file f in
		let s = List.fold_left (fun prev (rex, fn) -> qreplace ~rex ~templ:(try fn file with _ -> "_") prev) p l in
		let dir = FilePath.dirname s in
		mkdir dir;
		let s = s ^ "." ^ (get_extension f) in
		Unix.rename f.path s;
		close_file f;
		f
	in
	fun p -> Unix.handle_unix_error (process p)


(* Set tags *)

let set_title f s = tag_set_title f.file s; f
let set_album f s = tag_set_album f.file s; f
let set_artist f s = tag_set_artist f.file s; f
let set_genre f s = tag_set_genre f.file s; f
let set_comment f s = tag_set_comment f.file s; f
let set_year f s = tag_set_year f.file s; f
let set_track f s = tag_set_track f.file s; f


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
	!files

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

let usage () = print_endline "

tagedit /path/to/files filter1 filter2 ...

Filters:

	fix_cyrillic
	fix_chars=/()%...
	move_file='/new/path/%a/%y - %b/%n - %t'
	title=...
	album=...
	artist=...
	year=...
	track=...
	genre=...
	comment=...
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
				| "fix_cyrillic" -> FixCyrillic
				| "fix_chars" -> FixChars (get_v arg)
				| "move_file" -> MoveFile (get_v arg)
				| "title" -> Title (get_v arg)
				| "album" -> Album (get_v arg)
				| "artist" -> Artist (get_v arg)
				| "genre" -> Genre (get_v arg)
				| "comment" -> Comment (get_v arg)
				| "year" -> Year (get_i arg)
				| "track" -> Track (get_i arg)
				| _ -> error arg
		with
			| _ -> error arg
	in
	fun () ->
		match Array.to_list Sys.argv with
			| [] | [_] | [_; _] -> usage ()
			| _ :: path :: lst ->
				let filters = List.map parse_arg lst in
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

let process_file filters f =
	let _ = List.fold_left process_filter f filters in
	()

let () =
	let (path, filters) = parse_args () in
	if List.length filters = 0 then
		fatal "No filters specified";
	let files = get_files_list path in
	Printf.printf "Found %i files\n" (List.length files);
	List.iter (process_file filters) files

