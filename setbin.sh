#!/bin/bash
#
# SCRIPT: setbin
# AUTHOR: Scott Olsen
# DATE: 07.05.17 
# VERSION: 1.0 
#
# PLATFORM: Linux, OSX, mingw, cygwin, Solaris
#
# PURPOSE: Copies target files to an apropriate bin location depending on the user's enviornment.
#
# REV LIST:
#       DATE: Last Modified Date
#       BY: Scott Olse
#       MODIFICATION: Initial creation 
#       REPOSITORY URL: https://github.com/scolsen/misc_scripts 
#
# set -n # uncomment to check script syntax without
#        # executing the script.
#        # Be sure to recomment or the script will
#        # not exectue
# set -x # uncomment to debug this script
#
####################################
# VARIABLES
####################################
ENVIORNMENT=$(uname) #get the user enviornment through uname
BINPATH=hoopla #default path.
TARGETS=()

####################################
# FUNCTIONS
####################################

# match enviornments in order to use default path. If environement is not set, i.e. if uname did not work, prompt the user to select from list.
check_env(){
    case $ENVIRONMENT in 
        MINGW64_NT-6.1 | CYGWIN_NT-6.1-WOW)
        BINPATH="/usr/bin"
        ;;
        Linux )
        BINPATH="/usr/local/bin"
        ;;
    esac
}

# verify the default selected BINPATH is in fact in $PATH
# TODO: This is broken. Fix it.
verify_path(){
    VERIFICATION=$(echo $PATH | grep $BINPATH)
    echo $VERIFICATION
    if [[ $VERIFICATION != *"$BINPATH"* ]]; then
        OPTIONS=()
        NUM=0
        echo "Default bin location not found in path. Please specify a bin location from the following options stored in your current PATH:"
        IFS=\: read -a BINS <<< $(echo "$PATH")
        for i in "${BINS[@]}"; do
           OPTIONS=("${OPTIONS[@]}", $NUM) 
           let "NUM += 1"
        done
        for i in "${NUM[@]}"; do
            echo "${NUM[i]}) ${BINS[i]}" 
        done
    fi
    echo "Path Verified"
}

main(){
    verify_path
}

####################################
# MAIN
####################################
while test $# -gt 0
do
    case $1 in 
    --path | -p )
        shift
        BINPATH=$1
    ;;
    * )
       TARGETS=("${TARRGETS[@]}" "$1") 
    ;;
    esac
    shift
done
main
