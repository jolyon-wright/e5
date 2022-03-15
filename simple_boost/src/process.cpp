#include <iostream>

#if defined(_MSC_VER)
#pragma warning(push)
#pragma warning(disable : 4244)
#endif

#include <boost/process.hpp>
#if defined(_MSC_VER)
#pragma warning(push)
#endif

#include "stuff.h"

using namespace boost::process;


void
try_process_creation()
{
    ipstream pipe_stream;
    child c("gcc --version", std_out > pipe_stream);

    std::string line;

    while (pipe_stream && std::getline(pipe_stream, line) && !line.empty())
        std::cerr << line << std::endl;

    c.wait();
}
