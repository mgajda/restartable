 while inotifywait src/*.hs app/*.hs package.yaml ; do stack run -- legio-decima; done
