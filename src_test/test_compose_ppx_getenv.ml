open OUnit2

let test _ =
  assert_equal "BLARG" @@ if%const [%getenv "PPX_GETENV_CHECK"] = "42" then "BLARG" else 3

let suite = "Test ppx_const combination with ppx_getenv" >::: [
    "test_ppx_const_compose_ppx_getenv" >:: test;
  ]

let () =
  run_test_tt_main suite
