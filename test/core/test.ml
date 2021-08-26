open! Graphv_core_lib
(*

module DynBigarrayTest = struct
    open DynBigarray 

    let print a =
        Printf.printf "Size %d Cap %d\n" (length a) (capacity a);
        let idx = ref 0 in
        iter a ~f:(fun v -> 
            incr idx;
            Printf.printf "%.0f " v;
            if !idx mod 20 = 0 then (
                print_endline "";
            )
        );
        print_endline "";
    ;;

    let%expect_test "grow_test" =
        let arr = create 1 Bigarray.float32 0. in
        add arr 1.;
        add arr 2.;
        print arr;
        [%expect{|
          Size 2 Cap 2
          1 2 |}]
    ;;

    let%expect_test "grow_test2" =
        let arr = create 1 Bigarray.float32 0. in
        for i=0 to 99 do
            add arr (float i)
        done;
        print arr;
        [%expect{|
          Size 100 Cap 128
          0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
          20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39
          40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59
          60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
          80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 |}]
    ;;
end

module VertexBufferTest = struct
    open VertexBuffer

    let print a =
        Printf.printf "Size %d Cap %d\n" (num_verts a) (capacity a);
        let idx = ref 0 in
        iterv a ~f:(fun x y u v ->
            incr idx;
            Printf.printf "[%.0f %.0f %.0f %.0f] " 
                x y u v;
            if !idx mod 5 = 0 then (
                print_endline "";
            )
        );
        print_endline "";
    ;;

    let%expect_test "grow_test" =
        let v = create() in
        set v 0 1. 2. 3. 4.;
        set v 1 1. 2. 3. 4.;
        print v;
        [%expect {|
          Set 1.00 2.00 3.00 4.000000
          Idx 0 len 0 new 1
          Set 1.00 2.00 3.00 4.000000
          Idx 1 len 1 new 1
          Size 2 Cap 250
          [1 2 3 4] [1 2 3 4] |}]
    ;;

    let%expect_test "set test" =
        let v = create() in
        set v 10 1. 2. 3. 4.;
        print v;
        [%expect {|
          Set 1.00 2.00 3.00 4.000000
          Idx 10 len 0 new 11
          Size 11 Cap 250
          [0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]
          [0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]
          [1 2 3 4] |}]
    ;;

end
*)
