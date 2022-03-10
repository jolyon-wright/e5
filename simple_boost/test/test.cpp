#include <iostream>
#include <gtest/gtest.h>
#include "hello.h"


using namespace std;

TEST(example, simple_test)
{
  ASSERT_EQ(function_that_returns_42(), 42);
}


int main(int argc, char** argv)
{
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
