#!/bin/bash 
# Scaffold a node js project
# USAGE:
#	renameFiles	[ --help ]	
#			[ --project-name | -n <project_name>]
#			[ --bin | -b ]
#			[ --lib | -l ]
#                       [ --web | -w ]
#			[ --npm-verbose ]
#                       [ --debug ]
#
# NOTES:
#   This program scffolds a directory structure and initializes a node project using npm.	
#   Directory structure is dependant on the type of project specified by command line flags.	
#   The default project type is lib.	

#VARIABLES#
RUN_DIRECTORY=$(pwd) #get the directory this process was invoked in.
PROJECT_NAME="node-project"
DO_LIB=0
DO_BIN=0
DO_WEB=0
NPM_VERBOSE=0

#FUNCTIONS#

set_readme(){
    read -r -d '' README <<'EOF'
    # $PROJECT_NAME
    Provide a description of your project here.
    # Installation
    Detail how to install this software here.
EOF
}

make_lib(){
    mkdir $PROJECT_NAME
    mkdir $PROJECT_NAME/src
    touch $PROJECT_NAME/$PROJECT_NAME.js
    touch $PROJECT_NAME/README.md 
    cd $PROJECT_NAME
    echo "Initializing npm" 
    if [[ $NPM_VERBOSE == 1  ]]; then
        npm init 
    else 
        npm init --force
    fi
    echo "$README" > README.md
    git init
    git add .
    git commit -m "Initial commit"
}

main(){
    if [[ $DO_LIB == 1 ]]; then 
        set_readme
        make_lib
    fi
}

#MAIN#
while test $# -gt 0
do
    case $1 in
        --project-name | -p )
            shift
            PROJECT_NAME=$1
            ;;
        --lib | -l )
            DO_LIB=1
            ;;
        --bin | -b )
            DO_BIN=1
            ;;
        --web | -w )
            DO_WEB=1
            ;;
        --npm-verbose )
            NPM_VERBOSE=1
            ;;
        -* )
            error "Unrecognized option: $1"
            ;;
        * )
            break
            ;;
    esac
    shift
done
main
