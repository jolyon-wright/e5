/*
 * file-finder.cpp
 *
 * Copyright (c) 2022 Jolyon Wright.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
#include <iostream>
#include <filesystem>
#include <vector>
#include <string>

using namespace std;

typedef std::vector<string> string_vct;

int
scan_for_files(const char*       StartDir,
               const string_vct& MatchVct
               );

int
main(int    argc,
     char** argv
     )
{
  if (argc == 1) {
    cerr << "file-finder <dir> <substring1>[<substring2> [<substring3>]...]" << endl;
    return -1;
  }
  int ret_val{-1}; // default to error

  try {
    string_vct match_vct;

    while (--argc) {
      match_vct.push_back(argv[argc]);
    }

    ret_val = scan_for_files(argv[1],
                             match_vct
                             );

  }
  catch(const std::bad_alloc&) {
    cerr << "FATAL: Memory Allocation Failure" << endl;
  }
  catch(const std::exception& exception) {
    cerr << "FATAL: exception thrown:" <<
      exception.what() << endl;
  }
  catch(...) {
    cerr << "FATAL: unknown exception thrown:" << endl;
  }
  return ret_val;
}

int
scan_for_files(const char*       StartDir,
               const string_vct& MatchVct
               )
{
  int ret_val{0};

  cout << "scan " << StartDir << endl;

  for (auto v : MatchVct) {
    cout << v << endl;
  }

  return ret_val;
}
