#!/usr/local/bin/bash

#================================================================================ 
# Last Upate: Fri Oct  7 12:39:43 PDT 2016 
# Tue Nov 15 00:08:03 PST 2016  - add rsync, home dotfile to GoogleDrive/homedotfile
# 
# Script to manage all the small tasks such as editing and backup
#================================================================================ 
# all the colors in color.sh file
# [gf] open it 
# how to use in /Users/cat/myfile/script/jav.sh 
# e.g. printf "${FG_BR_RED}Hello World${RESET_ALL}\n"
# -------------------------------------------------------------------------------- 
# Tue May  7 16:46:25 2019 
# Add full path to ghc, Emacs can't find ghc from M-:
#================================================================================ 
# Sun Jul 28 18:21:09 2019 
# update to more generic file path
#================================================================================ 
# Tue 12 May 15:07:59 2020 
# Allow to add new symbol link name under $sym 
#================================================================================ 

# $(basename file.cpp) => file
#if [ "$#" -eq 0 ]; then
#else
#fi

#if [ "$?" -eq 0 ]; then
#else
#fi

#for var in $(ls) 
#do
#    $echo $var
#done 

# Mon Aug  5 14:27:58 2019 
# remove the code to read config.txt => osconfig.txt file
# change to read $b/publicfile/osconfig.txt

# copy binary into $HOME/myfile/mybin/xxxBin/xxx
# create symbol link in $sym/xxx ->  $HOME/myfile/mybin/xxxBin/xxx

function help(){
    printc 196 "help message"
}

source $HOME/myfile/bitbucket/script/AronLib.sh  

getpwd


# getName -> $ff/mybin/getName  is Haskell code
mybin=$HOME/myfile/mybin
fname=$(getName $PWD)
dir=${fname}Bin
bindir=$mybin/$dir

printc 200 "[fname=$fname]"
printc 200 "[dir=$dir]"
printc 200 "[bindir=$bindir]"


if [[ "$#" -eq 2 ]]; then
    if [[ "$1" == "in" ]]; then
        mkdir $bindir 
        stack install --local-bin-path $bindir 
        exeName=$(ls -A $bindir)

        cp ./config.txt $bindir 
        ls -lah $bindir

        cd $sym
        rm $sym/$exeName

        ln -s $bindir/$exeName $2
        ls -lah $mybin
        ls -lah $sym | grep $exeName

        exit_success
    elif [[ "$1" == "un" ]]; then
        exeName=$(ls -A $bindir)

        rm -rf $bindir
        cd $sym
        rm $sym/$exeName

        printc 200 "remove $bindir"
        printc 200 "remove $sym/$exeName"
        exit_success 
    else 
        printc 300 "Unsupported option"
        exit_failure 
    fi

elif [[ "$#" -eq 1 ]]; then

    if [[ "$1" == "in" ]]; then
        mkdir $bindir 
        stack install --local-bin-path $bindir 
        cp ./config.txt $bindir 

        exeName=$(ls -A $bindir)
        echo "$exeName"

        cd $sym
        rm $sym/$exeName

        ln -s $bindir/$exeName $exeName 
        ls -lah $mybin
        ls -lah $sym | grep $exeName

        exit_success
    elif [[ "$1" == "un" ]]; then
        exeName=$(ls -A $bindir)
        rm -rf $bindir
        cd $sym
        rm $sym/$exeName

        printc 200 "remove $bindir"
        printc 200 "remove $sym/$exeName"
        exit_success 
    else 
        printc 300 "Unsupported option"
        exit_failure 
    fi
else
    echo "install.sh in          => install"
    echo "install.sh in  newname => $sym/newname -> binaryfile"
    echo "install.sh un          => uninstall"
fi

