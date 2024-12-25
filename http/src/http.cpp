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
#include <string>
#include <iostream>
#include <cpr/cpr.h>

using namespace goldfish;
using namespace std;

static s7_pointer
f_http_head (s7_scheme* sc, s7_pointer args) {
  const char* url= s7_string (s7_car (args));
  cpr::Session session;
  session.SetUrl (cpr::Url (url));
  cpr::Response r= session.Head ();
  s7_pointer ht= s7_make_hash_table (sc, 8);
  s7_hash_table_set (sc, ht, s7_make_symbol (sc, "status-code"), s7_make_integer (sc, r.status_code));
  s7_hash_table_set (sc, ht, s7_make_symbol (sc, "url"), s7_make_string (sc, r.url.c_str()));
  s7_hash_table_set (sc, ht, s7_make_symbol(sc, "elapsed"), s7_make_real (sc, r.elapsed));
  s7_hash_table_set (sc, ht, s7_make_symbol (sc, "text"), s7_make_string (sc, r.text.c_str ()));
  s7_hash_table_set (sc, ht, s7_make_symbol (sc, "reason"), s7_make_string (sc, r.reason.c_str ()));

  return ht;
}

inline void
glue_http (s7_scheme* sc) {
  s7_pointer cur_env= s7_curlet (sc);

  const char* s_http_head = "g_http-head";
  const char* d_http_head = "(g_http-head url ...) => hash-table?";

  s7_define (sc, cur_env, s7_make_symbol (sc, s_http_head),
             s7_make_typed_function (sc, s_http_head, f_http_head, 1, 0, false,
                                     d_http_head, NULL));
}

int
main (int argc, char** argv) {
  string      gf_lib_dir  = find_goldfish_library ();
  const char* gf_lib      = gf_lib_dir.c_str ();
  s7_scheme* sc= init_goldfish_scheme (gf_lib);
  glue_http (sc);
  return repl_for_community_edition (sc, argc, argv);
}
