// example of boost program options/ logging

#include <iostream>
#include <fstream>
#include <boost/program_options.hpp>
#include <boost/log/core.hpp>
#include <boost/log/trivial.hpp>
#include <boost/log/expressions.hpp>
#include "stuff.h"
#include "methods.h"

using namespace boost;
using namespace std;
namespace       logging = boost::log;
namespace       po      = boost::program_options;

int
main(int    argc,
     char** argv
     )
{
    int                     err = 0;
    po::options_description desc("Options");
    string                  input_csv;
    string                  output_csv;
    vector<string>          config_file;
    vector<string>          expression_vct;
    int                     logLevel;
    vector<string>          show_vct;
    vector<string>          check_vct;

    return_42();

    // try_process_creation();


    // BOOST_LOG_TRIVIAL(trace) << "A trace severity message";
    // BOOST_LOG_TRIVIAL(debug) << "A debug severity message";
    // BOOST_LOG_TRIVIAL(info) << "An informational severity message";
    // BOOST_LOG_TRIVIAL(warning) << "A warning severity message";
    // BOOST_LOG_TRIVIAL(error) << "An error severity message";
    // BOOST_LOG_TRIVIAL(fatal) << "A fatal severity message";

    try {
        desc.add_options()
                ("help", "Print help messages")
                // ("input-csv,s", po::value<string>(&input_csv)->default_value("L197KeyCharacterGlossarySessionBySession_16J.csv"), "input vocab csv")
                // ("output-csv,o", po::value<string>(&output_csv)->default_value(""), "optional consolidated output csv")
                // ("config-file,c", po::value(&config_file), "config file.")
                // ("expression-pairs,e", po::value(&expression_vct)->multitoken(), "expression-pairs")
                // ("show-vct, s", po::value(&show_vct)->multitoken(), "show-vct")
                // ("check-vct, c", po::value(&check_vct)->multitoken(), "check-vct")
                ("log-level,t", po::value(&logLevel)->default_value((int)boost::log::trivial::trace), "trace level (0-5):-\n\
\t  trace   0 \n\
\t  debug   1 \n\
\t  info    2 \n\
\t  warning 3 \n\
\t  error   4 \n\
\t  fatal   5   \
")
                ;
        po::variables_map                  vm;
        po::positional_options_description p;

        p.add("input-csv", -1);

        po::store(po::command_line_parser(argc, argv).
                  options(desc).positional(p).run(),
                  vm
                  );
        // https://gist.github.com/ksimek/4a2814ba7d74f778bbee
        if (vm.count("config-file") > 0) {
            auto config_fnames = vm["config-file"].as<std::vector<std::string> >();

            for (size_t i = 0; i < config_fnames.size(); ++i) {
                std::ifstream ifs(config_fnames[i].c_str());

                if (ifs.fail()) {
                    std::cerr << "Error opening config file: " << config_fnames[i] << std::endl;
                    return __LINE__;
                }
                po::store(po::parse_config_file(ifs, desc), vm);
            }
        }
        po::notify(vm);
        logging::core::get()->set_filter(logging::trivial::severity >= logLevel);

        if (vm.count("help")) {
            std::cout << "chill." << std::endl
                      << desc << std::endl;
            return __LINE__;
        }        

        BOOST_LOG_TRIVIAL(fatal) << __FILE__ << "(" << __LINE__ << ") : there was not an error but lets pretend there was";
    }
    catch(po::error& e) {
        std::cerr << "ERROR: " << e.what() << std::endl << std::endl;
        std::cerr << desc << std::endl;
        err = __LINE__;
    }
    return err;
}
