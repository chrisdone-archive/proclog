# proclog

Put a proxy process in place of a program you want to run which will
log all stdin/stdout/stderr in the pipe along with timestamps.

Install the binary:

    $ stack install

Copy the binary to the program you want to log:

    $ cp $(which proclog) my-app

Make a config file under the same directory as `my-app` with the
extension `.args`: my-app.args

Put in the file

    --bin
    /path/to/real/my-app
    --log
    /path/to/log/file.txt

Adjust as needed.

Now when you run `my-app`, it'll transparently log all
stdin/stdout/stderr to `file.txt`.
