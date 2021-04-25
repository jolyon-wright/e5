#include <iostream>
#include <gtest/gtest.h>


#include "manual_test_fixture.h"

using namespace std;



TEST_F(manual_test_fixture, persistent_file)
{
    ASSERT_TRUE(be_jolyon());

}

int main(int argc, char** argv)
{
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
