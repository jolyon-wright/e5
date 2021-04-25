#pragma once

#include <gtest/gtest.h>


class manual_test_fixture : public ::testing::Test
{
  public:

    std::string get_os_info();

    bool
    be_jolyon();


    manual_test_fixture();
    void SetUp();
    void TearDown();
    ~manual_test_fixture();

    const std::string board_;
    const std::string distrib_;
    const std::string log_file_base_name_;
};






