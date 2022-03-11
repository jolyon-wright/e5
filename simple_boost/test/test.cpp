#include <iostream>
#include <gtest/gtest.h>
#include "methods.h"

using namespace std;

TEST(exaMPLE, simple_test)
{
  //  ASSERT_EQ(return_42, 42);
}


int main(int argc, char** argv)
{
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
