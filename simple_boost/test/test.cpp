#include <iostream>
#include <gtest/gtest.h>

using namespace std;

TEST(exaMPLE, simple_test)
{
    ASSERT_TRUE(false);
}


int main(int argc, char** argv)
{
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
