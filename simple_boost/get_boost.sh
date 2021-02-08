VERSION=1.74.0

wget https://dl.bintray.com/boostorg/release/1.74.0/source/boost_1_74_0.tar.gz
tar xvf boost_1_74_0.tar.gz
pushd boost_1_74_0
./bootstrap.sh
./b2 address-model=64 link=static runtime-link=static threading=multi
popd

cat << EOF >> ~/.bashrc
export BOOST_ROOT=/home/${USER}/boost_1_74_0
EOF
