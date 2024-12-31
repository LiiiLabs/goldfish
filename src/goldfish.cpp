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
#include <string>

#ifdef _MSC_VER

static bool add_overflow (s7_int A, s7_int B, s7_int *C) {
  *C = A + B;
  return ((A ^ B) >= 0) && ((A ^ *C) < 0);
}

static bool
int32_add_overflow (s7_int A, s7_int B, s7_int *C) {
  return add_overflow (A, B, C);
}

static bool
multiply_overflow (s7_int A, s7_int B, s7_int *C) {
  *C = A * B; return(false);
}

static bool
int32_multiply_overflow (s7_int A, s7_int B, s7_int *C) {
  return multiply_overflow (A, B, C);
}

static bool
subtract_overflow (s7_int A, s7_int B, s7_int *C) {
  *C = A - B; return(false);
}

#endif

int
main (int argc, char** argv) {
  std::string      gf_lib_dir  = goldfish::find_goldfish_library ();
  const char* gf_lib      = gf_lib_dir.c_str ();
  s7_scheme* sc= goldfish::init_goldfish_scheme (gf_lib);
  return goldfish::repl_for_community_edition (sc, argc, argv);
}

