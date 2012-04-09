task hello_world :: who=$someone {
  echo hello $who
}

// an anonymous configuration block
// no additional directory structure
config {
  someone=world
}