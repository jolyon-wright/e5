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

using namespace std::chrono_literals;
using namespace std;

// types
typedef std::queue<string>  string_queue;
typedef std::vector<thread> active_thread_vct;
typedef std::vector<string> string_vct;

struct termination_event
{
    // this simple structure is an attempt to encapsulate termination
    // functionality.

    bool               is_ready_;
    mutex              mutex_;          // a read/write lock
    condition_variable cond_;

    termination_event()
            : is_ready_(false)//, do_termination_(false)
    {}
};


struct substring_container
{
    // this simple structure is an attempt to encapsulate container access

    string_queue container_;  // this will contain all the matches
    shared_mutex swmr_mutex_; // a read/write lock
};


// methods

void
substring_adder_provider_thread(const char* StartDir, // the starting directory
                                const char* Pattern   // the substring pattern to find
                                );

// dumper thread proc
void
periodic_dumper_consumer();

// a helper function called from multiple threads
void dump_and_clear_records();


// globals (sorry)

substring_container g_container;
termination_event   g_terminate;


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

      // start the provider threads
      while (--argc > 1) {
          // start a thread for each pattern and add it to the thread vector
          active_thread.push_back(thread(substring_adder_provider_thread,
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
          // cout << "user says:" << user_input << endl;

          if (user_input == "dump") {
              dump_and_clear_records();
          }
          else if (user_input == "exit") {
              // we could signal termination here, but I think
              // being sequential is clearer...
              time_to_go = true;
          }
          else {
              cout << "please enter dump or exit" << endl;
          }
      }
      // now signal termination
      g_terminate.cond_.notify_all();

      { // just to scope the lock
          lock_guard<mutex> lk(g_terminate.mutex_); // block
          g_terminate.is_ready_       = true;
      }
      g_terminate.cond_.notify_all();

      // wait for all threads to terminate
      for (std::thread& thd : active_thread) {
          if (thd.joinable()) {
              thd.join();
          }
      }
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

void
substring_adder_provider_thread(const char* StartDir,
                                const char* Pattern
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
                // cout << "dir:" << entry << " (need to find " << Pattern << ")" << std::endl;
                const string& filename{entry.path().filename().string()};

                // do we care about this one?
                if (filename.find(Pattern) != string::npos) {
                    //std::unique_lock<std::shared_mutex> lk(g_container.swmr_mutex_); // hold for write
                    std::lock_guard<std::shared_mutex> lk(g_container.swmr_mutex_); // hold for write
                    //  we only want the filename (which is a path *not* a string!)
                    g_container.container_.push(filename);
                }
            }

            // time to terminate ?
            {
                unique_lock<mutex> lk(g_terminate.mutex_);

                if (g_terminate.cond_.wait_for(lk, 20ms, [] {return g_terminate.is_ready_;})) {
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

    // cout << "substring_adder_provider_thread terminating" << endl;
}

void
periodic_dumper_consumer()
{
    bool is_time_to_terminate{false};

    // do a timed wait for the termination request
    
    do {
        {
            unique_lock<mutex> lk(g_terminate.mutex_);

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

void dump_and_clear_records()
{
    cout << "dump_and_clear_records (called on thread " <<
            this_thread::get_id() << ")" <<
            endl;

    std::lock_guard<std::shared_mutex> lk(g_container.swmr_mutex_); // hold for write
    while (!g_container.container_.empty()) {
        cout << __FUNCTION__ << ":" << g_container.container_.front() << endl;
        g_container.container_.pop();
    }
}
