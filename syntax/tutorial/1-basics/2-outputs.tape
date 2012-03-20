// 2) Writing output files
//
// * Ducttape will assign the paths for the output files
//   as something like ./hello-world-2/x and ./hello-world-2/y.txt
// * The environment variables x and y_txt will be set by
//   ducttape before calling bash
// * Note that bash disallows variables containing .
[hello_world_2] > x y_txt {
  echo writing files $x and $y...
  echo hello > $x
  echo world > $y_txt
}
