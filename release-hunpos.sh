#!/bin/bash 
set -e 
VERSION=1.0
TARGETS="trainer.native tagger.native" 
FLAGS=" -libs str -I util -I hunpos -I hunpos/lib"
OCAMLBUILD=ocamlbuild

clean ()
{

  $OCAMLBUILD -clean;
  
    
} 
ocb() 
{ 
  $OCAMLBUILD $FLAGS $TARGETS 
} 

usage()
{
    echo "usage: ./build [build|clean|release win | release macosx | relelase linux]"
}

copy-release-files ()
{

    echo "warning: NO README FILE"

}


release-nx ()
{ 

  cd _build
  
  DIR=hunpos-$VERSION-$1
  rm -rf $DIR; mkdir $DIR
  cp ../trainer.native $DIR/trainer
  cp ../tagger.native $DIR/tagger
  copy-release-files $DIR
  
  tar cvfz $DIR.tar.gz $DIR
  scp $DIR.tar.gz hp@staro.mokk.bme.hu:/public/Tool/Hunpos/Pre/

}
release()
{


  if [ $# -eq 0 ]; then 
    echo "please give a platform";
    usage
  else

    ocb;
    if [ $1 == "win" ]; then
        echo "win release";
    else
       release-nx $1;
    fi
  fi
}


rule() { 

  case $1 in 
   clean) clean;; 
   
   build) ocb  ;;
   
   release) shift; release $*;; 
   
  
   
   *) echo "Unknown action $1";; 

   esac; 
} 

if [ $# -eq 0 ]; then 
  usage ;
else 
  rule $*; 
fi 
