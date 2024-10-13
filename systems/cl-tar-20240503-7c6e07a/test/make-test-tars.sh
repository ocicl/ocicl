#! /bin/sh

DIR="$(mktemp -d)"

MTIME="Fri Aug 13 09:05:46.75 PM EDT 2021"

touch_file () {
    touch -h -a --date "$MTIME" "$1"
}

touch_files () {
    touch_file "$DIR/a.txt"
    touch_file "$DIR/a-symlink.txt"
    touch_file "$DIR/a-hardlink.txt"
    touch_file "$DIR/fifo"
    sudo touch -a --date "$MTIME" "$DIR/sda1"
    sudo touch -a --date "$MTIME" "$DIR/tty0"
    # touch_file "$DIR/sparse.txt"
}

echo "Hello, world!" > "$DIR/a.txt"
ln -s a.txt "$DIR/a-symlink.txt"
ln "$DIR/a.txt" "$DIR/a-hardlink.txt"
mkfifo "$DIR/fifo"
sudo mknod "$DIR/sda1" b 8 1
sudo mknod "$DIR/tty0" c 4 0
# truncate -s 5M "$DIR/sparse.txt"

run_tar () {
    files="a.txt a-symlink.txt a-hardlink.txt"
    sparse_option=""

    if [ "$1" != "v7" ]; then
        files="$files fifo sda1 tty0"
    fi

    if [ "$1" = "gnu" ] || [ "$1" = "pax" ]; then
        files="$files"
        # sparse_option="-S"
    fi

    tar -H "$1" -C "$DIR" \
        --group=root --owner=root \
        -b 20 \
        $sparse_option \
        --mtime "Fri Aug 13 09:05:46.5 PM EDT 2021" \
        -cf "$1.tar" $files
}

touch_files
run_tar ustar
touch_files
run_tar pax
touch_files
run_tar gnu
touch_files
run_tar v7

sudo rm -rf "$DIR"
