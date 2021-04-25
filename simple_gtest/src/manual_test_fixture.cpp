#include <iostream>
#include <vector>
#include <map>
#include <gtest/gtest.h>
#include <fstream>
#include <sys/utsname.h>
#include "manual_test_fixture.h"

#include <boost/process.hpp>
#include <boost/asio/ip/host_name.hpp>
#include <boost/algorithm/string.hpp>

using namespace boost::process;
using namespace std;

typedef vector<string> stringVct;

string manual_test_fixture::get_os_info() {
    std::string Result;
    std::map<std::string, std::string> value_map;

    // read through the /etc/os-release file and
    // figure what version we are running on
    ifstream command_file("/etc/os-release");
    if(command_file.good()) {
        std::string line;

        while(std::getline(command_file, line)) {
            if(line.size() == 0)  break;

            std::vector<std::string> valid_keys = {"ID", "VERSION_ID"};
            std::string key = line.substr(0, line.find('='));

            if(std::find(valid_keys.begin(), valid_keys.end(), key) != valid_keys.end()) {
                std::string value = line.substr(line.find('=') + 1);
                boost::erase_all(value, "\"");
                value_map.insert(std::make_pair(key, value));
            }
        }

        Result = value_map["ID"] + '-' + value_map["VERSION_ID"];
    }
    else {
        Result = "unknown";
    }
    return Result; // RVO
}
manual_test_fixture::manual_test_fixture()
        : board_(boost::asio::ip::host_name()),
          distrib_(get_os_info()),
          log_file_base_name_(board_ + "_" + distrib_)
{}

void manual_test_fixture::SetUp()
{
    // code here will execute just before the test ensues
}

void manual_test_fixture::TearDown()
{
    // code here will be called just after the test completes
    // ok to through exceptions from here if need be
}

manual_test_fixture::~manual_test_fixture()
{
    // cleanup any pending stuff, but no exceptions allowed
}


bool
manual_test_fixture::be_jolyon()
{
    bool     ret_val = true;
    const string test_name(::testing::UnitTest::GetInstance()->current_test_info()->name());
    string   log_name(log_file_base_name_  + "_" + test_name + ".log");

    ofstream f(log_name);

    // check we can access the files
    if(!f.good()) {
        cerr << "failed to create log file (" + log_name + ")" << endl;
        return false;
    }

    ifstream command_file(test_name + ".txt");

    if(!command_file.good()) {
        cerr << "failed to open command file (" + test_name + ".txt)" << endl;
        f << "failed to open command file (" + test_name + ".txt)" << endl;
        return false;
    }

    std::vector<std::string> commands;
    std::string              line;

    // note the commands
    while(std::getline(command_file, line)) {
        if(line.size() == 0)   break;

        commands.push_back(line);
    }

    // attempt to execute each one, noting the result
    for(const auto& Command : commands) {
        ipstream    pipe_stream;
        try {
            f << "$ " + Command << std::endl; // note this before an exception can be thrown

            child       cproc(Command, std_out > pipe_stream);
            std::string line;

            while (pipe_stream && std::getline(pipe_stream, line) && !line.empty())
                f << line << std::endl;

            cproc.wait();

            // if a command failed bail
            if (cproc.exit_code() != 0) {
                f << ": command failed! Aborting" << endl;
                cerr << "$ " + Command << ": command failed! Aborting" << endl;
                ret_val = false;
                break;
            }
        }
        catch (...)
        {
            // if a command was invalid we should note it
            f << ": command failed! Aborting" << endl;
            cerr << "$ " + Command << ": command failed! Aborting" << endl;
            ret_val = false;
            break;
        }
    }

    return ret_val;
}

