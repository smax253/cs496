open OUnit2
open Ast
open Ds
open Interp

(* A few test cases *)
let tests = [
  "int"  >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "22"));
  "add"  >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "11+11"));
  "adds" >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "(10+1)+(5+6)"));
  "let"  >:: (fun _ -> assert_equal (Ok (NumVal 22)) (interp "let x=22 in x"));
  "lets" >:: (fun _ -> assert_equal (Ok (NumVal 22))
                 (interp "let x = 0 in let x = 22 in x"));

      (* pair *)
    "pair1 " >::
        (fun _ -> assert_equal
            (Ok (PairVal (NumVal 3, NumVal 4)))
            (interp "pair(3, 4)"));

    "pair2 " >::
        (fun _ -> assert_equal
            (Ok (PairVal ((PairVal (NumVal 3, NumVal 4)), NumVal 5)))
            (interp "pair(pair(3, 4), 5)"));

    "pair3 " >::
        (fun _ -> assert_equal
            (Ok (PairVal (BoolVal true, NumVal 3)))
            (interp "pair(zero?(0), 3)"));

    "unpair1 "  >::
        (fun _ -> assert_equal
            (Ok (NumVal 3))
            (interp "unpair (a, b) = pair(1, 2) in a + b"));
]

let _ = run_test_tt_main ("suite" >::: tests)
