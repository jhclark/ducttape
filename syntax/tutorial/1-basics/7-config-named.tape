task hello_world :: who=$someone {
  echo hello $who
}

// an named configuration block
// all tasks will be nested under an additional subdirectory called "World"
config World {
  someone=world
}

global {
  ducttape_structure=flat
}
