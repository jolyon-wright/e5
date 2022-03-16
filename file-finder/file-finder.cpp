/*
 * file-finder.cpp
 *
 * Copyright (c) 2022 Jolyon Wright. (jolyon.wright@gmail.com)
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
 *
 * Please see README.org for commentary.
 *
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

using namespace std::chrono_literals;
using namespace std;

struct termination_event
{
    // this simple structure is an attempt to encapsulate termination
    // functionality.

    bool               is_ready_;
    mutex              mutex_;
    condition_variable cond_;

    termination_event()
            : is_ready_(false)
    {}
};

struct substring_container
{
    // this simple structure is an attempt to encapsulate container access

    queue<string> container_;   // this will contain all the matches
    mutex         mutex_;       // a read/write lock

    // this is the threadproc
    void
    finder_proc(const char* StartDir, // the starting directory
                const char* Pattern   // the substring pattern to find
                );
};

// methods:-

// dumper thread proc
void
periodic_dumper_consumer();

// a helper function called from multiple threads
void
dump_and_clear_records();


// globals (sorry):-

substring_container g_container;
termination_event   g_terminate;


// implementation:-
int
main(int    argc,
     char** argv
     )
{
  int ret_val{-1}; // default to error

  if (argc == 1) {
    cerr << "file-finder <dir> <substring1>[<substring2> [<substring3>]...]" << endl;
    return ret_val;
  }
  try {
      vector<thread> active_thread;

      // start the provider threads
      while (--argc > 1) {
          // start a thread for each pattern and add it to the thread vector
          active_thread.push_back(thread(&substring_container::finder_proc, 
                                         &g_container,
                                         argv[1],   // this is the start directory
                                         argv[argc] // this is the pattern
                                         )
                                  );
          // cout << "startdir:" << string(argv[1]) << " pattern:" << string(argv[argc]) << endl;
      }

      // start the periodic dumper thread
      active_thread.push_back(thread(periodic_dumper_consumer));

      // now we will block, waiting for user input
      bool time_to_go{false};
      while (!time_to_go) {
          string user_input;
          cin >> user_input;

          // the requirement states that our verbs should start with a capital,
          // however show some mercy on the user by accepting lower case too
          if (user_input == "Dump" || user_input == "dump") {
              dump_and_clear_records();
          }
          else if (user_input == "Exit" || user_input == "exit") {
              // we could signal termination here, but I think
              // being sequential is clearer...
              time_to_go = true;
          }
          else {
              cout << "please enter Dump or Exit" << endl;
          }
      }
      // now signal termination
      g_terminate.cond_.notify_all();

      { // just to scope the lock
          lock_guard<mutex> lk(g_terminate.mutex_); // block
          g_terminate.is_ready_ = true;
      }
      g_terminate.cond_.notify_all();

      // wait for all threads to terminate
      for (thread& thd : active_thread) {
          if (thd.joinable()) {
              thd.join();
          }
      }
      ret_val = 0;
  }
  catch(const bad_alloc&) {
      cerr << "FATAL: Memory Allocation Failure" << endl;
  }
  catch(const exception& exception) {
      cerr << "FATAL: exception thrown:" <<
              exception.what() << endl;
  }
  catch(...) {
      cerr << "FATAL: unknown exception thrown:" << endl;
  }
  return ret_val;
}

void
substring_container::finder_proc(const char* StartDir, // the starting directory
                                 const char* Pattern   // the substring pattern to find
                                 )
{
    assert(StartDir);
    assert(Pattern);

    // find all matches for this pattern and put them in the container
    // but (!!!) be prepared to abort if the termination event is set!

    try {
        for (const auto& entry : filesystem::recursive_directory_iterator(StartDir)) {
            // we only care about files
            if (filesystem::is_regular_file(entry)) {
                // cout << "dir:" << entry << " (need to find " << Pattern << ")" << endl;

                //  we only want the filename (which is a path *not* a string!)
                const string& filename{entry.path().filename().string()};

                // do we care about this one?
                if (filename.find(Pattern) != string::npos) {
                    lock_guard<mutex> lk(mutex_); // hold for write
                    container_.push(filename);
                }
            }

            // time to terminate ?
            {
                unique_lock<mutex> lk(g_terminate.mutex_);
                if (g_terminate.cond_.wait_for(lk, 20ms, [] {return g_terminate.is_ready_; })) {
                    // yes!
                    break;
                }
            }
        }
    }
    catch (const filesystem::filesystem_error& exception) {
        cerr << "ERROR : filesystem exception thrown:" << exception.what() << endl;
    } catch (...) {
        cerr << "ERROR : unexpected exception thrown." << endl;
    }
}

void
periodic_dumper_consumer()
{
    bool is_time_to_terminate{false};

    do {
        {
            unique_lock<mutex> lk(g_terminate.mutex_);

            // do a timed wait for the termination request
            if (g_terminate.cond_.wait_for(lk, 10s, [] {return g_terminate.is_ready_;})) {
                // we have the condition variable
                is_time_to_terminate = true;
            }
        }
        if (!is_time_to_terminate) {
            dump_and_clear_records();
        }
    } while (!is_time_to_terminate);
}

void
dump_and_clear_records()
{
    cout << "dump_and_clear_records (called on thread " <<
            this_thread::get_id() << ")" <<
            endl;
    {
        lock_guard<mutex> lk(g_container.mutex_); // hold for write
        while (!g_container.container_.empty()) {
            cout << __FUNCTION__ << ":" << g_container.container_.front() << endl;
            g_container.container_.pop();
        }
    }
}
