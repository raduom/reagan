FROM ubuntu:14.04

WORKDIR /root

RUN apt-get update \
 && apt-get -y install curl build-essential diffutils libuuid-tiny-perl libxml-libxml-perl libstring-escape-perl libgetopt-declare-perl opam libgmp-dev libmpfr-dev libffi-dev coreutils unifdef python-jinja2 python-pygments software-properties-common sudo vim git m4 clang pkg-config libpcre3-dev unzip pslist \
 && add-apt-repository ppa:avsm/ppa -y \
 && add-apt-repository ppa:webupd8team/java -y \
 && apt-get update \
 && echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections \
 && echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections \
 && apt-get install -y oracle-java8-installer oracle-java8-set-default sudo \
 && wget -O rv-match.jar https://runtimeverification.com/match/download/linux \
 && printf "\n1\nY\n1\n" | java -jar rv-match.jar \
 && rm rv-match.jar 

RUN wget https://embed.cs.utah.edu/csmith/csmith-2.3.0.tar.gz \
 && tar xzvf csmith-2.3.0.tar.gz \
 && cd csmith-2.3.0 \
 && ./configure && make -j 4 && make install \
 && make clean \
 && wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin \
 && apt-get install unzip \
 && opam init \
 && opam switch 4.05.0 \
 && eval `opam config env` \
 && opam install coq=8.7.1 \
 && opam install menhir 

RUN wget http://compcert.inria.fr/release/compcert-3.2.tgz \
 && tar xzvf compcert-3.2.tgz \
 && cd CompCert-3.2 \
 && eval `opam config env` \
 && ./configure x86_64-linux \
 && make all \
 && make install \
 && make clean 

ENV CPATH=":/usr/local/include/csmith-2.3.0" \
    PATH="/root/.local/bin:.:${PATH}"

RUN wget -qO- https://get.haskellstack.org/ | sh \
 && stack --resolver lts-10.4 setup

COPY . reagan/

RUN cd /root/reagan \
 && stack install \
