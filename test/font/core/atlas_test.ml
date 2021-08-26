open Graphv_font
open Graphv_core_lib
open Atlas

let print (a : t) =
    Printf.printf "W: %d\nH: %d\n\n" a.width a.height;
    DynArray.iter a.nodes ~f:(fun node ->
        Printf.printf "[x: %d\n y: %d\n width: %d]\n\n"
            node.x
            node.y
            node.width
    );
    print_endline ""
;;

let print_option = function
    | None -> print_endline "None"
    | Some (x,y) -> Printf.printf "x %d y %d\n" x y
;;

let node_lookup = [|
    '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9';
    'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j';
|]

let get_node_char idx =
    node_lookup.(idx mod Array.length node_lookup)
;;

let added_lookup = [|
    'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J';
    'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T';
|]

let get_added_char idx =
    added_lookup.(idx mod Array.length added_lookup)
;;

let print_as_grid ?(added=[]) atlas =
    let w = atlas.width in
    let h = atlas.height in
    let arr = Array.make (w*h) ' ' in

    let index = ref 0 in
    for _=0 to w-1 do
        print_char '_'
    done;
    print_endline "";
    DynArray.iter atlas.nodes ~f:(fun node ->
        let h = atlas.height - node.y in
        let x = node.x in
        let w = x + node.width in
        for y=0 to h-1 do
            for x=x to w-1 do
                arr.(x + y*atlas.width) <- get_node_char !index
            done;
        done;
        incr index;
    );

    List.iteri (fun idx (x, y, w, h) -> 
        for y=y to y+h-1 do
            for x=x to x+w-1 do
                let y = atlas.height - y - 1 in
                arr.(x + y*atlas.width) <- get_added_char idx
            done
        done;
    ) added;

    for y=0 to h-1 do
        for x=0 to w-1 do
            print_char arr.(x + y*w)
        done;
        print_endline ""
    done;
    for _=0 to w-1 do
        print_char '-'
    done;
    print_endline "\n";
;;

let%expect_test "create" =
    let atlas = create ~width:1024 ~height:1024 ~ncount:100 in
    print atlas;
    [%expect {|
      W: 1024
      H: 1024

      [x: 0
       y: 0
       width: 1024] |}]
;;

let%expect_test "add_rect" =
    let atlas = create ~width:1024 ~height:1024 ~ncount:100 in
    let res = add_rect atlas 100 100 in
    print atlas;
    print_option res;
    [%expect {|
      W: 1024
      H: 1024

      [x: 0
       y: 100
       width: 100]

      [x: 100
       y: 0
       width: 924]


      x 0 y 0 |}]
;;

let build_added_list lst w h =
    List.map (function
        | None -> failwith "expected some"
        | Some (x, y) -> (x, y, w, h)
    ) lst
;;

let%expect_test "add_rect overflow" =
    let atlas = create ~width:10 ~height:10 ~ncount:100 in
    let w = 6 in
    let h = 5 in
    let res1 = add_rect atlas w h in
    let res2 = add_rect atlas w h in
    print_as_grid ~added:(build_added_list [res1; res2] w h) atlas;
    print atlas;
    print_option res1;
    print_option res2;
    [%expect {|
      __________
      BBBBBB1111
      BBBBBB1111
      BBBBBB1111
      BBBBBB1111
      BBBBBB1111
      AAAAAA1111
      AAAAAA1111
      AAAAAA1111
      AAAAAA1111
      AAAAAA1111
      ----------

      W: 10
      H: 10

      [x: 0
       y: 10
       width: 6]

      [x: 6
       y: 0
       width: 4]


      x 0 y 0
      x 0 y 5 |}]
;;

let%expect_test "add_rect_many" =
    let atlas = create ~width:50 ~height:20 ~ncount:100 in
    let added = ref [] in
    for i=0 to 19 do
        let h = (i mod 10 + 1) in
        Printf.printf "%c h %d " (get_added_char i) h;
        match add_rect atlas 5 h with
        | None -> failwith "expected some"
        | Some (x, y) as res ->
            print_option res;
            added := (x, y, 5, h) :: !added
    done;
    let added = List.rev !added in
    print_as_grid ~added atlas;
    print atlas;
    [%expect {|
      A h 1 x 0 y 0
      B h 2 x 5 y 0
      C h 3 x 10 y 0
      D h 4 x 15 y 0
      E h 5 x 20 y 0
      F h 6 x 25 y 0
      G h 7 x 30 y 0
      H h 8 x 35 y 0
      I h 9 x 40 y 0
      J h 10 x 45 y 0
      K h 1 x 0 y 1
      L h 2 x 0 y 2
      M h 3 x 5 y 2
      N h 4 x 10 y 3
      O h 5 x 0 y 4
      P h 6 x 15 y 4
      Q h 7 x 5 y 5
      R h 8 x 20 y 5
      S h 9 x 25 y 6
      T h 10 x 10 y 7
      __________________________________________________
      00000111112222233333444445555566666777778888899999
      00000111112222233333444445555566666777778888899999
      00000111112222233333444445555566666777778888899999
      0000011111TTTTT33333444445555566666777778888899999
      0000011111TTTTT33333444445555566666777778888899999
      0000011111TTTTT3333344444SSSSS66666777778888899999
      0000011111TTTTT3333344444SSSSS66666777778888899999
      0000011111TTTTT33333RRRRRSSSSS66666777778888899999
      00000QQQQQTTTTT33333RRRRRSSSSS66666777778888899999
      00000QQQQQTTTTT33333RRRRRSSSSS66666777778888899999
      00000QQQQQTTTTTPPPPPRRRRRSSSSS666667777788888JJJJJ
      OOOOOQQQQQTTTTTPPPPPRRRRRSSSSS6666677777IIIIIJJJJJ
      OOOOOQQQQQTTTTTPPPPPRRRRRSSSSS66666HHHHHIIIIIJJJJJ
      OOOOOQQQQQNNNNNPPPPPRRRRRSSSSSGGGGGHHHHHIIIIIJJJJJ
      OOOOOQQQQQNNNNNPPPPPRRRRRFFFFFGGGGGHHHHHIIIIIJJJJJ
      OOOOOMMMMMNNNNNPPPPPEEEEEFFFFFGGGGGHHHHHIIIIIJJJJJ
      LLLLLMMMMMNNNNNDDDDDEEEEEFFFFFGGGGGHHHHHIIIIIJJJJJ
      LLLLLMMMMMCCCCCDDDDDEEEEEFFFFFGGGGGHHHHHIIIIIJJJJJ
      KKKKKBBBBBCCCCCDDDDDEEEEEFFFFFGGGGGHHHHHIIIIIJJJJJ
      AAAAABBBBBCCCCCDDDDDEEEEEFFFFFGGGGGHHHHHIIIIIJJJJJ
      --------------------------------------------------

      W: 50
      H: 20

      [x: 0
       y: 9
       width: 5]

      [x: 5
       y: 12
       width: 5]

      [x: 10
       y: 17
       width: 5]

      [x: 15
       y: 10
       width: 5]

      [x: 20
       y: 13
       width: 5]

      [x: 25
       y: 15
       width: 5]

      [x: 30
       y: 7
       width: 5]

      [x: 35
       y: 8
       width: 5]

      [x: 40
       y: 9
       width: 5]

      [x: 45
       y: 10
       width: 5] |}]
;;
