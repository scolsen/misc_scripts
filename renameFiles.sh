#!/bin/bash 
# rename files. Using a csv file. 
#TODO Check: SRC_NAMES must be a csv
# Directory inputs must NOT contain spaces.
# USAGE:
#	renameFiles	[ --help ]	
#			[ --new-file-name | -n <csvFile>]
#			[ --old-file-directory | -d <directoryOfFilesToRename>]
#			[ --extension | -e <extensionFilter>]
#			[ --debug ]
#
# NOTES:
#	Input should match the following format:
#		<currentFileName>,<newFileName>
#	Some examples:
#		543345,JohnSmith
#		myOldFile,hello
#	This process does not clean file names. As a result, file names with spaces will likely cause unexpected results or errors. 
#	Use the -e option to specify an extension filter--only files matching the given extension will be renamed. If an extension filter
#	is not specified, all files in the target folder matching the old name values in the source csv will be renamed, regardless of extension.
#	The extension option is not required. Its value defaults to 'jpg'

HELPTXT="
USAGE:
	renameFiles	[ --help ]	
			[ --new-file-name | -n <csvFile>]
			[ --old-file-directory | -d <directoryOfFilesToRename>]
			[ --extension | -e <extensionFilter>]
			[ --debug ]

OPTIONS:
	--help | -h 
		Displays the program help.
	--new-file-name | -n
		Source csv file containing comma delimited fields in the format <oldFileName>,<newFileName>
	--old-file-directory | -d
		Directory containing the files to be renamed. File names must match the <oldFileName> value in the input csv file in order to be changed.
	--extension | -e
		Specifies an extension filter. Only files with extensions matching the value of this argument will be changed. Defaults to 'jpg'.
	--debug
		Run debug mode. Debug output is echoed. No files will be renamed. 
NOTES:
	Input should match the following format:
		<currentFileName>,<newFileName>
	Some examples:
		543345,JohnSmith
		myOldFile,hello
	This process does not clean file names. As a result, file names with spaces will likely cause unexpected results or errors. 
	Use the -e option to specify an extension filter--only files matching the given extension will be renamed. If an extension filter
	is not specified, all files in the target folder matching the old name values in the source csv will be renamed, regardless of extension.
	The extension option is not required. Its value defaults to 'jpg'
EXAMPLES:
	renameFiles -n Dropbox/MyFiles/StudentNames.csv -d Dropbox/studentphotos/ -e .png"

#VARIABLES#
FILESARR=()
DEBUG=0
EXT="jpg"

#FUNCTIONS#

show_help(){
	echo "$HELPTXT"
	exit
}

process_files(){
	if [[ $SRC_NAMES == *" "* ]]; then
		echo "Error. Argument values must not contain spaces."
		exit
	fi
	if [[ $TAR_DIR == *" "*  ]]; then
		echo "Error. Argument values must not contain spaces."
		exit
	fi
	if [[ $SRC_NAMES != *".csv" ]]; then
		echo "Error. Input must be a csv file"
	fi

	CONTENT="$(cat $SRC_NAMES | tr "\r" "\n")"
	TARGET="$(ls TAR_DIR)"
	
	echo "$SRC_NAMES"
	echo "$TAR_DIR"

	while IFS="," read -r line 
	do
		if [[ $DEBUG == 1 ]]; then
			echo "Line: $line"
		fi
		echo "[renameFiles]: Reading CSV file..."
		FILESARR=("${FILESARR[@]}" "$line")
	done <<< "$CONTENT" 
	if [[ $DEBUG == 1 ]]; then
		echo "FILES ARR: ${FILESARR[@]}"
	fi	

	for i in ${FILESARR[@]}
	do
		OLD="$(echo $i | sed 's/\(.*\),.*$/\1/')"
		NEW="$(echo $i | sed 's/^.*,\(.*\)$/\1/')"
		if [[ $DEBUG == 1 ]]; then
			echo "Old File Name: $OLD New File Name: $NEW"
		fi
		
		if [[ -f "${TAR_DIR}${OLD}.$EXT" ]]; then
			if [[ $DEBUG == 1 ]]; then
				echo "$OLD is a file."
			else
				echo "[renameFiles]: Renaming files..."
				mv $TAR_DIR$OLD.$EXT $TAR_DIR$NEW.$EXT
			fi
		fi
	done
}

if [[ $# == 0 ]] || [[ $# -lt 1 ]]; then
	echo -e "Error you must specify the following arguments:\n\t--new-file-name <csvFile>\n\t--old-file-directory <locationOfFilesToRename>"
	exit
fi

while test $# -gt 0
do
	case $1 in
		--help | -h )
		show_help
		;;
		--new-file-name | -n )
		shift
		SRC_NAMES=$1
		;;
		--old-file-directory | -d )
		shift
		TAR_DIR=$1
		;;
		--extension | -e )
		shift
		EXT=$1
		;;
		--debug )
		DEBUG=1
		;;
		* )
		show_help 
		;;
	esac 
	shift 
done

if [[ $EXT == "."* ]]; then 
	EXT="$(echo $EXT | sed 's/\.\(.*\)/\1/')"
fi
if [[ $DEBUG == 1 ]]; then
	echo $EXT
fi
process_files
