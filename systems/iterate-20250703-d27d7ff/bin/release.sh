#!/bin/sh

VERSION=$1

ARCHIVE_NAME=iterate-"$VERSION".tar.gz
ARCHIVE_PATH="/project/iterate/public_html/releases"
ARCHIVE_FULLNAME="$ARCHIVE_PATH/$ARCHIVE_NAME"

if [ -e "$ARCHIVE_FULLNAME" ]; then
  echo "$ARCHIVE_FULLNAME already exists"
  exit -1
fi

echo "\nCreating release '$VERSION'\n\nDon't forget to tag the darcs repo!\nOutput will be $ARCHIVE_FULLNAME.\n\nPress enter to continue..."
read tmp
cd /project/iterate/darcs
cp -r iterate iterate-$VERSION
tar --exclude="iterate*/_darcs*" --exclude="iterate*/.*" --exclude="iterate*/bin" -zvcf "$ARCHIVE_FULLNAME" iterate-$VERSION
rm -rf iterate-$VERSION
cd "$ARCHIVE_PATH"
gpg --detach-sign --armor "$ARCHIVE_NAME"
ln -fs "$ARCHIVE_NAME" iterate-current.tar.gz
ln -fs "$ARCHIVE_NAME".asc iterate-current.tar.gz.asc
