FROM gcr.io/oss-fuzz-base/base-builder-rust
RUN apt-get update && apt-get install -y make autoconf automake libtool
COPY . $SRC/robot-repo-automaton
WORKDIR $SRC/robot-repo-automaton
COPY .clusterfuzzlite/build.sh $SRC/
