//
// Copyright (C) 2024 The Goldfish Scheme Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations
// under the License.
//

#include "goldfish.hpp"
#include "s7.h"

#include <filesystem>
#include <iostream>
#include <sstream>
#include <vector>

using std::cerr;
using std::cout;
using std::endl;
using std::string;
using std::vector;
using std::filesystem::exists;
using std::filesystem::path;

void display_help() {
  cout << "Goldfish Scheme " << goldfish_version << " by LiiiLabs" << endl;
  cout << "--version\t"
       << "display version" << endl;
  cout << "-e       \t"
       << "-e '(+ 1 2)'" << endl;
  cout << "filename \t"
       << "Load the scheme code and evaluate it" << endl;
}

int main(int argc, char **argv) {
  // Check if the standard library and boot.scm exists
  const path gf_root = path(argv[0]).parent_path().parent_path();
  const path gf_lib = gf_root / "goldfish";
  const path gf_boot = gf_lib / "scheme" / "boot.scm";
  if (!exists(gf_lib)) {
    cerr << "The load path for Goldfish Scheme Standard Library does not exist"
         << endl;
  }
  if (!exists(gf_boot)) {
    cerr << "The boot.scm for Goldfish Scheme does not exist" << endl;
  }

  // Init the underlying S7 Scheme and add the load_path
  s7_scheme *sc;
  sc = s7_init();
  s7_load(sc, gf_boot.string().c_str());
  s7_add_to_load_path(sc, gf_lib.string().c_str());

  // Glues for the Standard Library
  glue_scheme_time(sc);

  // Command options
  vector<string> args(argv + 1, argv + argc);
  if (args.size() == 0) {
    display_help();
  } else if (args.size() == 1) {
    if (args[0] == "--version") {
      cout << "Goldfish Scheme " << goldfish_version << "by LiiiLabs" << endl;
      cout << "based on S7 Scheme " << S7_VERSION << "(" << S7_DATE << ")"
           << endl;
    } else {
      if (!s7_load(sc, args[0].c_str())) {
        cerr << "error" << endl;
      }
    }
  } else if (args.size() == 2 && args[0] == "-e") {
    s7_pointer x = s7_eval_c_string(sc, args[1].c_str());
    cout << s7_object_to_c_string(sc, x) << endl;
  } else {
    cerr << "Invalid command line options!" << endl;
    display_help();
  }
  return 0;
}
