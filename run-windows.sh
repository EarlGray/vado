#!/bin/bash
set -e

MINGWDIR="/c/Users/$USERNAME/AppData/Local/Programs/stack/x86_64-windows/msys2-20180531"

mkdir -p bin/
for LIB in libbrotlicommon.dll libbrotlidec.dll \
			libcairo-2.dll libfontconfig-1.dll libfreetype-6.dll \
			libglib-2.0-0.dll libgraphite2.dll libharfbuzz-0.dll \
			libjpeg-8.dll libpixman-1-0.dll \
			libpng16-16.dll libtiff-5.dll libwebp-7.dll libzstd.dll \
			SDL2.dll SDL2_image.dll
do
	test -e "bin/$LIB" || cp "$MINGWDIR/mingw64/bin/$LIB" bin/
done

cp "$(stack path --local-install-root)/bin/vado.exe" bin/
cd bin/
exec ./vado.exe
