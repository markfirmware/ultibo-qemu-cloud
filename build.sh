#!/bin/bash

docker run -it --rm -v "$(pwd):/code" -w "/code" -e "HOME=/tmp" -u $UID:$GID -p 8000:8000 markfirmware/ultibo-1.3.271-x64-1.0.0 elm make Main.elm
sed -i 's/^body { /\0overflow: hidden; /' index.html
