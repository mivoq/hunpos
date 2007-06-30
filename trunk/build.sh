#!/bin/bash 
set -e 
VERSION=1.0
TARGETS="trainer.native tagger.native" 
FLAGS=" -libs str -I util -I hunpos -I hunpos/lib"
OCAMLBUILD=ocamlbuild

clean ()
{

  $OCAMLBUILD $FLAGS -clean;
  
    
} 

# generates a version.ml file which is compiled into
# the source
make-version-ml()
{

    echo "let version = \""$(VERSION)"\"" > version.ml
	echo "let date = \""`date`"\"" >> version.ml

}
ocb() 
{ 
  $OCAMLBUILD $FLAGS $TARGETS 
} 

usage()
{
    echo "usage: ./build [ build | clean | release linux | release win | release macosx ]"
}

copy-release-files ()
{
  echo "NO README FILE"
}


release-nx ()
{ 

  cd _build
  
  DIR=hunpos-$VERSION-$1
  rm -rf $DIR; mkdir $DIR
  if [ $1 == "win" ]; then
    EXT=".exe"
    if [ ! -e /bin/cygwin1.dll ]; then
      echo "cygwin1.dll dynamic library not found."
      echo "it will be not packaged with the release."
    else
      echo "packaging the cygwin1.dll dynamic library."
      cp /bin/cygwin1.dll $DIR
    fi
  fi
  cp hunpos/trainer.native $DIR/hunpos-train$EXT
  cp hunpos/tagger.native $DIR/hunpos-tag$EXT
  copy-release-files $DIR
  
  echo "compressing the release..."
  if [ $1 == "win" ]; then
    ARCHIVE=$DIR.zip
    rm -f $ARCHIVE
    zip -r $ARCHIVE $DIR
  else
    ARCHIVE=$DIR.tgz
    rm -f $ARCHIVE
    tar cvfz $ARCHIVE $DIR
  fi

  echo
  echo "the release is ready at _build/$ARCHIVE"
  echo

  echo "uploading the release to its proper place at the mokk ftp server..."
  echo "(obviously, this final step will fail if you are not the maintainer."
  echo "in this case, just press Ctrl-C and enjoy the release.)"
  echo
  echo -n "please enter your kruso.mokk.bme.hu username: "
  read USERNAME
  scp $ARCHIVE $USERNAME@kruso.mokk.bme.hu:/public/Tool/Hunpos/Pre/
  ssh $USERNAME@kruso.mokk.bme.hu chmod -w /public/Tool/Hunpos/Pre/$ARCHIVE
}
release()
{
  if [ $# -eq 0 ]; then 
    echo "please specify a platform";
    usage
  else
    case $1 in
    linux) ;;
    macosx) ;;
    win) ;;
    *) echo "unknown platform $1" ; exit ;;
    esac;

    ocb;
    release-nx $1;
  fi
}


rule() { 

  case $1 in 
   clean) clean;; 
   
   build) ocb  ;;
   
   release) shift; release $*;; 
   
  
   
   *) echo "unknown action $1";; 

   esac; 
} 

if [ $# -eq 0 ]; then 
  usage ;
else 
  rule $*; 
fi 
