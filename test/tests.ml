open Test_modules

let () =
  Alcotest.run "All tests"
    [ ("Binary search", Test_binary_search.suite)
    ; ("Merge sort", Test_merge.suite)
    ]
