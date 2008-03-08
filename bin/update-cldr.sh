#!/bin/bash

PROJECT_HOME="`dirname $0`/.."

absolutize ()
{
  if [ ! -d "$1" ]; then
    echo
    echo "ERROR: '$1' doesn't exist or not a directory!"
    exit -1
  fi

  cd "$1"
  echo `pwd`
  cd - >/dev/null
}

PROJECT_HOME=`absolutize "$PROJECT_HOME"`

echo "Assumin cl-l10n is in: '$PROJECT_HOME'"

if [ ! -d "$PROJECT_HOME" ]; then
    echo Something is not ok, there is no "$PROJECT_HOME" directory?! Bailing out...
    exit 1
fi

cd "$PROJECT_HOME/cldr/"

read -p "About to recursively remove everything in `pwd`/, continue (y/n)?"

if [ "$REPLY" = "y" ]; then
  rm -rf *
else
  exit 2
fi

wget http://unicode.org/Public/cldr/1.5.1/core.zip
unzip core.zip
rm core.zip

echo Done.
