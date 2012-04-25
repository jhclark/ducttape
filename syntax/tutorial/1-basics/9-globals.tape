task hello_world :: who=$someone {
  echo hello $who
}

// a block of global variables
// these are variables that should
// be shared among *all* configs
global {
  someone=world

  # Remember, the ducttape_structure global is a special directive to ducttape
  ducttape_structure=flat
}
