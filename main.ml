module Split = struct
  let numget x =
    let x' = int_of_float x in
    if abs_float (float_of_int x' -. x) <= epsilon_float
    then Pdf.Integer x'
    else Pdf.Real x
  
  let box x y x' y' = 
    Pdf.Array [numget x; numget y; numget x'; numget y']
  
  let split_h (x,y,x',y') =
    let y2 = y +. y' /. 2. in
    [x,y2,x',y'; x,y,x',y2]
  
  let split_v (x,y,x',y') =
    let x2 = x +. x' /. 2. in
    [x,y,x2,y'; x2,y,x',y']
  
  let split_hr (x,y,x',y') =
    let y2 = y +. y' /. 2. in
    [x,y,x',y2; x,y2,x',y']
  
  let split_vr (x,y,x',y') =
    let x2 = x +. x' /. 2. in
    [x2,y,x',y'; x,y,x2,y']
  
  let rec chain = function
    | f :: fs  -> (fun x -> List.concat (List.map f (chain fs x)))
    | []       -> (fun x -> [x])
  
  let mediabox_split f = function
    | Pdf.Array [x; y; x'; y'] ->
        let x, y, x', y' =
          Pdf.getnum x, Pdf.getnum y, Pdf.getnum x', Pdf.getnum y'
        in
        List.map (fun (a,b,c,d) -> box a b c d) (f (x,y,x',y'))
    | _ -> raise (Invalid_argument "mediabox_split")
  
  let try_split f o =
    try mediabox_split f o
    with Invalid_argument _ -> [o]

  let h, v, hr, vr =
    let wrap f =
      let page_split page =
        let boxes = try_split f page.Pdfpage.mediabox in
        List.map (fun box -> { page with Pdfpage.mediabox = box }) boxes
      in
      fun pages -> List.flatten (List.map page_split pages)
    in
    wrap split_h, wrap split_v, wrap split_hr, wrap split_vr
end

type page = Pdfpage.t

type command =
  | Read_pdf of string
  | Variable of string
  | Transform of (page list -> page list)
  | Set_variable of string
  | Sub of command list

module Args = struct
  let output = ref ""

  let set_output s =
    if !output = "" then output := s
    else failwith ("\"" ^ s ^ "\": you can specify only one output file")

  let commands = ref []
  let stack = ref []
  
  let push_stack () =
    stack := !commands :: !stack;
    commands := []
  
  let get_commands () =
    List.rev !commands
  
  let pop_stack () =
    match !stack with
      | [] -> failwith "Unmatched ')'"
      | cmds :: stack' ->
          stack := stack';
          commands := Sub (get_commands ()) :: cmds
  
  let push_instr cmd = commands := cmd :: !commands

  let instr cmd = Arg.String (fun s -> push_instr (cmd s))
  let transform f = Arg.Unit (fun () -> push_instr (Transform f))
  let transform' f = Arg.String (fun s-> push_instr (Transform (f s)))
  
  let filter_pages range pages =
    let sep, len = String.index range '-', String.length range in
    let lo, hi = String.sub range 0 sep, String.sub range (succ sep) (len - succ sep) in
    let as_int s = try int_of_string s with Invalid_argument _ -> -1 in
    let lo, hi = as_int lo, as_int hi in
    let rec list_drop n = function
      | x :: xs when n > 0 -> list_drop (pred n) xs
      | xs -> xs
    in
    let rec list_take n = function
      | x :: xs when n > 0 -> x :: list_take (pred n) xs
      | _ -> []
    in
    let pages = list_drop lo pages in
    if hi > -1
    then list_take (succ hi - lo) pages
    else pages

  let set_mediabox s pages =
    Scanf.sscanf s "%f,%f,%f,%f" (fun x y x' y' ->
        List.map (fun page ->
            { page with Pdfpage.mediabox =
              match page.Pdfpage.mediabox with
              | Pdf.Array [x0; y0; x'0; y'0] ->
                let x0, y0, x'0, y'0 =
                  Pdf.getnum x0, Pdf.getnum y0, Pdf.getnum x'0, Pdf.getnum y'0
                in
                Pdf.(Array [Real (x0 +. x); Real (y0 +. y); 
                            Real (x'0 +. x'); Real (y'0 +. y')])
              | box -> box
            })
          pages
      )

  let print_mediabox pages =
    List.iter (fun {Pdfpage.mediabox} ->
      match mediabox with
      | Pdf.Array [x; y; x'; y'] ->
        let x, y, x', y' =
          Pdf.getnum x, Pdf.getnum y, Pdf.getnum x', Pdf.getnum y'
        in
        Printf.printf "%f,%f,%f,%f\n" x y x' y'
      | _ -> ())
      pages;
    pages

  let rec odd_pages = function
    | x :: _ :: pages -> x :: odd_pages pages
    | pages -> pages

  let args = Arg.align [
    "(", Arg.Unit push_stack, " Enter page substream";
    ")", Arg.Unit pop_stack, " Leave page substream";
  
    "--output", Arg.String set_output, " ";
    "-o", Arg.String set_output, " Set output file";
  
    "--input", instr (fun s -> Read_pdf s), " ";
    "-i", instr (fun s -> Read_pdf s), " Append pages from pdf file";
  
    "--split-horz", transform Split.h, " ";
    "-h", transform Split.h, " Split horizontally";
  
    "--split-vert", transform Split.v," ";
    "-v", transform Split.v, " Split vertically";
  
    "--split-horz-reverse", transform Split.hr," ";
    "-hr", transform Split.hr, " Split horizontally, from right to left";
  
    "--split-vert-reverse", transform Split.vr," ";
    "-vr", transform Split.vr, " Split vertically, from right to left";
  
    "--pages", transform' filter_pages, " ";
    "-p", transform' filter_pages, " Keep pages in given range. Ranges are specified as <min-bound>-<maxbound>, where bounds are optional";
  
    "--odd", transform odd_pages, " Keep every odd page";
  
    "-@", instr (fun s -> Set_variable s), " ";
    "--set", instr (fun s -> Set_variable s), " Save current page stream for later reuse";

    "--mediabox", transform print_mediabox, " Print mediabox for each page";

    "--adjust-mediabox", transform' set_mediabox, "X,Y,X',Y' Shift mediabox dimension of each page by specified amount";
  ]
  let variable s = push_instr (Variable s)

  let usage = Printf.sprintf
    "PdfMagick!\nUsage: %s -o output.pdf ...commands..."
    (Filename.basename Sys.argv.(0))

  let parse () =
    Arg.parse args variable usage;
    if !stack <> [] then failwith "Unclosed parenthesis";
    if !output = "" then prerr_endline "WARNING: No output file specified (use -o <out>.pdf).";
    let result = get_commands () in
    stack := []; commands := [];
    !output, result

  let help () = Arg.usage args usage
end

let plausible_name n =
  (Filename.check_suffix (String.lowercase n) ".pdf") && Sys.file_exists n

let list_files commands =
  let rec extract_names acc commands =
    List.fold_left
    begin fun acc -> function
      | Sub commands -> extract_names acc commands
      | Read_pdf s -> s :: acc
      | Variable s when plausible_name s -> s :: acc
      | _ -> acc
    end acc commands
  in
  List.rev (extract_names [] commands)

module StringMap = Map.Make (String)

type state = {
  variables : page list StringMap.t;
  files : Pdf.t StringMap.t;
  pages : page list
}

let empty_state = {
  variables = StringMap.empty;
  files = StringMap.empty;
  pages = [];
}

let rec execute state = function
  | Read_pdf name -> { state with pages = state.pages @ 
                       Pdfpage.pages_of_pagetree
                         (StringMap.find name state.files) }
  | Variable name -> { state with pages = state.pages @ 
                         (StringMap.find name state.variables) }
  | Transform f -> { state with pages = f state.pages }
  | Set_variable name ->
      { state with variables = StringMap.add name state.pages state.variables }
  | Sub cmds ->
      let state' = List.fold_left execute { state with pages = [] } cmds in
      { state' with pages = state.pages @ state'.pages }

let main () =
  try
    let output, commands = Args.parse () in
    let names = list_files commands in
    let pdfs = List.map (Pdfread.pdf_of_file None None) names in
    let pdfs = Pdf.renumber_pdfs pdfs in
    let files = List.fold_left2
      (fun files name file -> StringMap.add name file files)
      StringMap.empty names pdfs
    in
    let minor = List.fold_left max 0 (List.map (fun p -> p.Pdf.minor) pdfs) in
    let doc = List.fold_left (execute pdfs)  { empty_state with files } commands in
    if output <> "" then
      begin
        let pdf = ref (Pdf.empty ()) in
        List.iter (Pdf.objiter (fun k v -> ignore (Pdf.addobj_given_num !pdf (k, v)))) pdfs;
        let pdf, pagetree_num = Pdfpage.add_pagetree doc.pages !pdf in
        let pdf = Pdfpage.add_root pagetree_num [] pdf in
        let pdf = {pdf with Pdf. major = 1; minor } in
        Pdf.remove_unreferenced pdf;
        Pdfwrite.pdf_to_file pdf output
      end
  with Failure s ->
    Args.help ();
    prerr_endline s


let () = main ()
