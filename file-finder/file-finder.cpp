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
#include <condition_variable>
#include <mutex>
#include <thread>
#include <vector>
#include <queue>
#include <string>
#include <cassert>
#include <shared_mutex>

using namespace std;

// types
typedef std::queue<string>  string_queue;
typedef std::vector<thread> active_thread_vct;


typedef std::vector<string> string_vct;

// void thread_function();

struct substring_container
{
    bool         is_ready_;
    string_queue container_;
    shared_mutex container_mutex; // a read/write lock
    // condition ?

    substring_container()
            : is_ready_(false)
    {}
};

// globals (sorry)

substring_container g_container;



// methods
void
substring_adder_provider_thread(const char* StartDir,
                                const char* Pattern
                                )
{
    assert(StartDir);
    assert(Pattern);

    // cout << "startdir:" << string(StartDir) << " pattern:" << string(Pattern) << endl;

    // find all matches for this pattern and put them in the container
    // but (!!!) be prepared to abort if the termination event is set!

    try {
        for (const auto& entry : filesystem::recursive_directory_iterator(StartDir)) {
            // we only care about files
            if (filesystem::is_regular_file(entry)) {
                std::cout << "dir:" << entry << " (need to find " << Pattern << ")" << std::endl;
                // we only want the filename (which is a path *not* a string!)
                g_container.container_.push(entry.path().filename().string());
            }

            // time to terminate ?
        }
    }
    catch (const filesystem::filesystem_error& exception) {
        cerr << "ERROR : filesystem exception thrown:" << exception.what() << endl;
    }
}


// int
// scan_for_files(const char*       StartDir,
//                const string_vct& MatchVct
//                );


// implementation
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
      active_thread_vct active_thread;

      //string_vct match_vct;

      while (--argc > 1) {
          active_thread.push_back(thread(substring_adder_provider_thread,
                                         argv[1],   // this is the start directory
                                         argv[argc] // this is the pattern
                                         )
                                  );

          cout << "startdir:" << string(argv[1]) << " pattern:" << string(argv[argc]) << endl;
          //      match_vct.push_back(argv[argc]);
      }


      // wait for all threads to terminate
      for (std::thread& thd : active_thread) {
          if (thd.joinable()) {
              thd.join();
          }
      }

      // ret_val = scan_for_files(argv[1],
      //                          match_vct
      //                          );
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

  // do the search on the main thread



  // std::thread threadObj(thread_function);
  // threadObj.join();

  // puts("im back");


  return ret_val;
}


// void thread_function()
// {
//     for(int i = 0; i < 10000; i++);

//     std::cout<<"thread function Executing"<<std::endl;
// }
