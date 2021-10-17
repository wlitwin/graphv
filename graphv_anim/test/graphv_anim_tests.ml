open Graphv_anim

let string_of_status = function
    | Done -> "Done"
    | Canceled -> "Canceled"
;;

let update str t =
    Printf.printf "%s %.2f\n%!" str t
;;

let complete str id status =
    Printf.printf "%s %d %s\n%!" str id (string_of_status status)
;;

let print_end_results (t : Driver.t) =
    Printf.printf "Active %d = 0\n%!" (Driver.active_count t);
    Printf.printf "Pending %d = 0\n%!" (Driver.active_count t);
;;

let run_simple_test (anim : anim) (ticks : float list) =
    let driver = Driver.create() in
    Printf.printf "Starting %d\n%!" (Driver.start driver anim);
    List.iter (fun tick ->
        Driver.tick driver tick;
    ) ticks;
    print_end_results driver 
;;

let basic time prefix =
    create time ~complete:(complete (prefix ^ " Complete"))
        (update (prefix ^ " Update"))
;;

let%expect_test "Create 1s no repeat" = 
    let anim = create 1. ~complete:(complete "Complete") (update "Update") in
    run_simple_test anim [1.];
    [%expect {|
      Starting 0
      Update 1.00
      Complete 0 Done
      Active 0 = 0
      Pending 0 = 0 |}]
;;

let%expect_test "Create serial 1s no repeat one animation" =
    let anim = serial
        ~complete:(complete "Serial Complete") 
    [
        basic 1. "Basic"
    ] in
    run_simple_test anim [1.];
    [%expect {|
      Starting 0
      Basic Update 1.00
      Basic Complete 0 Done
      Serial Complete 0 Done
      Active 0 = 0
      Pending 0 = 0 |}]
;;

let%expect_test "Create nested serial" =
    let anim = serial ~complete:(complete "Outer serial complete")
    [
        serial ~complete:(complete "Inner serial Complete")
        [
            basic 1. "Basic"
        ]
    ] in
    run_simple_test anim [1.];
    [%expect {|
      Starting 0
      Basic Update 1.00
      Basic Complete 0 Done
      Inner serial Complete 0 Done
      Outer serial complete 0 Done
      Active 0 = 0
      Pending 0 = 0 |}]
;;

let%expect_test "Multiple serial" =
    let anim = serial ~complete:(complete "Serial complete")
    [
        basic 1. "Basic 1";
        basic 1. "Basic 2";
    ] in
    run_simple_test anim [0.5; 0.5; 0.5; 0.5];
    [%expect {|
      Starting 0
      Basic 1 Update 0.50
      Basic 1 Update 1.00
      Basic 1 Complete 0 Done
      Basic 2 Update 0.00
      Basic 2 Update 0.50
      Basic 2 Update 1.00
      Basic 2 Complete 0 Done
      Serial complete 0 Done
      Active 0 = 0
      Pending 0 = 0 |}]
;;

let%expect_test "Multiple nested serial" =
    let anim = serial ~complete:(complete "Outer serial complete")
    [
        serial ~complete:(complete "Inner serial complete 1") [
            basic 1. "Basic 1";
            basic 1. "Basic 2";
        ];
        serial ~complete:(complete "Inner serial complete 2") [
            basic 1. "Basic 3";
            basic 1. "Basic 4";
        ];
    ] in
    run_simple_test anim [1.; 1.; 1.; 1.];
    [%expect {|
      Starting 0
      Basic 1 Update 1.00
      Basic 1 Complete 0 Done
      Basic 2 Update 0.00
      Basic 2 Update 1.00
      Basic 2 Complete 0 Done
      Inner serial complete 1 0 Done
      Basic 3 Update 0.00
      Basic 3 Update 1.00
      Basic 3 Complete 0 Done
      Basic 4 Update 0.00
      Basic 4 Update 1.00
      Basic 4 Complete 0 Done
      Inner serial complete 2 0 Done
      Outer serial complete 0 Done
      Active 0 = 0
      Pending 0 = 0 |}]
;;

let%expect_test "Basic repeat" =
    let anim = create 1. 
        ~repeat:(Count 2)
        ~complete:(complete "Basic complete")
        (update "Basic update")
    in
    run_simple_test anim [1.; 1.;1.];
    [%expect {|
      Starting 0
      Basic update 1.00
      Basic complete 0 Done
      Basic update 1.00
      Basic complete 0 Done
      Active 0 = 0
      Pending 0 = 0 |}]
;;

let%expect_test "Basic backwards" =
    let anim = create 1.
        ~direction:Backward
        ~complete:(complete "Basic complete")
        (update "Basic update")
    in
    run_simple_test anim [0.; 0.25; 0.25; 0.25; 0.25; 0.25];
    [%expect {|
      Starting 0
      Basic update 1.00
      Basic update 0.75
      Basic update 0.50
      Basic update 0.25
      Basic update 0.00
      Basic complete 0 Done
      Active 0 = 0
      Pending 0 = 0 |}]
;;

let%expect_test "Basic backward repeat" =
    let anim = create 1.
        ~direction:Backward
        ~repeat:(Count 2)
        ~complete:(complete "Basic complete")
        (update "Basic update")
    in
    run_simple_test anim [
        0.; 0.25; 0.25; 0.25; 0.25; 
        0.25; 0.25; 0.25; 0.25; 0.25;
    ];
    [%expect {|
      Starting 0
      Basic update 1.00
      Basic update 0.75
      Basic update 0.50
      Basic update 0.25
      Basic update 0.00
      Basic complete 0 Done
      Basic update 0.75
      Basic update 0.50
      Basic update 0.25
      Basic update 0.00
      Basic complete 0 Done
      Active 0 = 0
      Pending 0 = 0 |}]
;;

let%expect_test "Basic mirror forward" =
    let anim = create 1.
        ~direction:(Mirror Forward)
        ~complete:(complete "Basic complete")
        (update "Basic update")
    in
    run_simple_test anim [0.; 0.25; 0.25; 0.25; 0.25; 0.25];
    [%expect {|
      Starting 0
      Basic update 0.00
      Basic update 0.25
      Basic update 0.50
      Basic update 0.75
      Basic update 1.00
      Basic complete 0 Done
      Active 0 = 0
      Pending 0 = 0 |}]
;;

let%expect_test "Basic mirror backward" =
    let anim = create 1.
        ~direction:(Mirror Backward)
        ~complete:(complete "Basic complete")
        (update "Basic update")
    in
    run_simple_test anim [0.; 0.25; 0.25; 0.25; 0.25; 0.25];
    [%expect {|
      Starting 0
      Basic update 1.00
      Basic update 0.75
      Basic update 0.50
      Basic update 0.25
      Basic update 0.00
      Basic complete 0 Done
      Active 0 = 0
      Pending 0 = 0 |}]
;;

let%expect_test "Basic mirror forward repeat" =
    let anim = create 1.
        ~direction:(Mirror Forward)
        ~repeat:(Count 4)
        ~complete:(complete "Basic complete")
        (update "Basic update")
    in
    run_simple_test anim [
        0.; 0.25; 0.25; 0.25; 0.25;
        0.; 0.25; 0.25; 0.25; 0.25;
        0.; 0.25; 0.25; 0.25; 0.25;
        0.; 0.25; 0.25; 0.25; 0.25;
    ];
    [%expect {|
      Starting 0
      Basic update 0.00
      Basic update 0.25
      Basic update 0.50
      Basic update 0.75
      Basic update 1.00
      Basic complete 0 Done
      Basic update 1.00
      Basic update 0.75
      Basic update 0.50
      Basic update 0.25
      Basic update 0.00
      Basic complete 0 Done
      Basic update 0.00
      Basic update 0.25
      Basic update 0.50
      Basic update 0.75
      Basic update 1.00
      Basic complete 0 Done
      Basic update 1.00
      Basic update 0.75
      Basic update 0.50
      Basic update 0.25
      Basic update 0.00
      Basic complete 0 Done
      Active 0 = 0
      Pending 0 = 0 |}]
;;

let%expect_test "Basic mirror backward repeat" =
    let anim = create 1.
        ~direction:(Mirror Backward)
        ~repeat:(Count 4)
        ~complete:(complete "Basic complete")
        (update "Basic update")
    in
    run_simple_test anim [
        0.; 0.25; 0.25; 0.25; 0.25;
        0.; 0.25; 0.25; 0.25; 0.25;
        0.; 0.25; 0.25; 0.25; 0.25;
        0.; 0.25; 0.25; 0.25; 0.25;
    ];
    [%expect {|
      Starting 0
      Basic update 1.00
      Basic update 0.75
      Basic update 0.50
      Basic update 0.25
      Basic update 0.00
      Basic complete 0 Done
      Basic update 0.00
      Basic update 0.25
      Basic update 0.50
      Basic update 0.75
      Basic update 1.00
      Basic complete 0 Done
      Basic update 1.00
      Basic update 0.75
      Basic update 0.50
      Basic update 0.25
      Basic update 0.00
      Basic complete 0 Done
      Basic update 0.00
      Basic update 0.25
      Basic update 0.50
      Basic update 0.75
      Basic update 1.00
      Basic complete 0 Done
      Active 0 = 0
      Pending 0 = 0 |}]
;;

let%expect_test "Basic infinite" =
    let anim = create 1.
        ~repeat:Infinite
        ~complete:(complete "Basic complete")
        (update "Basic update")
    in
    run_simple_test anim [
        0.; 0.25; 0.25; 0.25; 0.25;
        0.; 0.25; 0.25; 0.25; 0.25;
        0.; 0.25; 0.25; 0.25; 0.25;
        0.; 0.25; 0.25; 0.25; 0.25;
    ];
    [%expect {|
      Starting 0
      Basic update 0.00
      Basic update 0.25
      Basic update 0.50
      Basic update 0.75
      Basic update 1.00
      Basic complete 0 Done
      Basic update 0.00
      Basic update 0.25
      Basic update 0.50
      Basic update 0.75
      Basic update 1.00
      Basic complete 0 Done
      Basic update 0.00
      Basic update 0.25
      Basic update 0.50
      Basic update 0.75
      Basic update 1.00
      Basic complete 0 Done
      Basic update 0.00
      Basic update 0.25
      Basic update 0.50
      Basic update 0.75
      Basic update 1.00
      Basic complete 0 Done
      Active 0 = 0
      Pending 0 = 0 |}]
;;
