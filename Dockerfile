FROM openjdk:8u141-jdk
RUN touch /usr/lib/jvm/java-8-openjdk-amd64/release

ENV SBT_VERSION 0.13.15

WORKDIR /root

# Install sbt
RUN \
  curl -L -o sbt-$SBT_VERSION.deb https://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb && \
  dpkg -i sbt-$SBT_VERSION.deb && \
  rm sbt-$SBT_VERSION.deb && \
  apt-get update && \
  apt-get install sbt && \
  sbt sbtVersion

# Install mpfr
RUN apt-get install libmpfr4

# Install Z3
RUN apt-get install libgomp1 z3 

# Install dReal
RUN \
  curl -fsL https://github.com/dreal/dreal3/releases/download/v3.16.09.01/dReal-3.16.09.01-linux.tar.gz | tar xz && \
  ln -s ~/dReal-3.16.09.01-linux/bin/dReal /usr/bin/dReal
