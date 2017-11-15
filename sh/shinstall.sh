#!/bin/bash
#
# SCRIPT: shinstall
# AUTHOR: Scott Olsen
# DATE: 9.12.17
# VERSION: 1.0 
#
# PLATFORM: Linux, gitBash
#
# PURPOSE: Searches the user's path and attempts to install a target sh script in the user's path.
#
# REV LIST:
#       DATE: 9.13.17 
#       BY: Scott Olsen 
#       REPOSITORY URL: https://github.com/scolsen/utils
#
# set -n # uncomment to check script syntax without
#        # executing the script.
#        # Be sure to recomment or the script will
#        # not exectue
# set -x # uncomment to debug this script
#

# Cosmetic
# -----------------------------------

nc="\033[0m"
green="\033[32m"
lred="\033[1;4;31m"
bold="\033[1m"
bluebg="\033[48;5;25m"

yesno="(${green}y${nc}/${lred}n${nc}):"


# Help
# -----------------------------------

HELP="\t${bold}shinstall${nc}: install shell scripts or executables to PATH.
\t${bold}Usage${nc}: shinstall [OPTIONS...] [FILES...]

\t--help     Display this help and exit.
\t-b, --bin  Specify an explicit path to which targets files will be copied."

# Variables
# -----------------------------------

BINPATH="/usr/local/bin"
TARGETS=()
UNINSTALLED=()

# Functions
# -----------------------------------

exiter(){
    echo "Exiting."
    exit 1
}

help(){
    echo -en "$HELP"
    exiter
}

is_installed(){
   arr=("$@")
   echo "Checking if target files are already installed."
   for i in "${arr[@]}"
   do
    if [ ! -f "$BINPATH/$i" ]; then
        UNINSTALLED=("${UNINSTALLED[@]}" "$i")
    else 
        echo "$i already exists at $BINPATH"
    fi
   done
   echo -en "Attempting to install the following files to $BINPATH: "
   for i in "${UNINSTALLED[@]}"
    do
        echo -en "$i\n"
    done
    echo -en "Do you want to install the preceeding files to $BINPATH? ${yesno} "
    read yn
    if [ $yn == "y" ] || [ $yn == "yes" ] || [ $yn == "Y" ] || [ $yn == "Yes" ]; then
        echo "Copying files."
    else
      exiter  
    fi
}

verify_path(){
    if [[ $PATH != *"$BINPATH"* ]]; then
        echo -en "Specified bin location not stored on path.\nAre you sure you want to install the script here? ${yesno} "
        read yn
        if [ $yn == "y" ] || [ $yn == "yes" ] || [ $yn == "Y" ] || [ $yn == "Yes" ]; then
            echo "Verifying path exists."
            if [ ! -d "$BINPATH" ]; then
                echo "Target install directory $BINPATH does not exist or is not a directory."
                exiter
            fi
        else
            exiter
        fi
    fi
}

copy_targets(){
    if [ -f "$1" ]; then
        FILE="$1"
        NOPATH="${FILE##*/}"
        cp "$1" "$BINPATH/$NOPATH"
        if [[ $1 == *.sh ]]; then
            DROPEXT=$(echo "$1" | sed "s/\.sh//")
            mv "$BINPATH/$1" "$BINPATH/$DROPEXT"
        fi
        echo -en "Copied $1 to $BINPATH\n"
    else 
        echo -en "$1 does not exist or is not a file.\n"
    fi
    echo "Copied $1 to $BINPATH"
}

# Main
# -----------------------------------
main() {
    verify_path
    is_installed "${TARGETS[@]}"
    for i in "${UNINSTALLED[@]}"
    do
        copy_targets "$i"
    done
}

while test $# -gt 0
do 
    case $1 in
    --bin | -b )
        shift
        BINPATH=$1
    ;;
    --help )
        help
    ;;
    * )
        TARGETS=("${TARGETS[@]}" "$1")
    ;;
    esac
    shift
done
main
