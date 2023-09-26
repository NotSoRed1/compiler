FLAGS=$(cat compile_flags.txt)



echo "=============== making the build directory ================="
rm -rf build
mkdir build
echo "================ compiling the source code ================="
clang $FLAGS -O2 src/**.c src/utils/**.c -o build/compiler
echo "============ copying the standard library files ============"
cp -r std build/std
echo "=============== finished building the project =============="

