# proclog

Put a proxy process in place of a program you want to run which will
log all stdin/stdout/stderr in the pipe along with timestamps.

Kind of like `tee` but with timestamps and a bit easier to throw 
all logs in one file and also into separate files.

Install the binary:

    $ stack install

Copy the binary to the program you want to log:

    $ cp $(which proclog) my-app

Make a config file under the same directory as `my-app` with the
extension `.args`: my-app.args

Put in the file (args are line separated)

    --bin
    /path/to/real/my-app
    --log
    /path/to/log/file.txt

Adjust as needed.

Now when you run `my-app`, it'll transparently log all
stdin/stdout/stderr to `file.txt`.

If desired, you can also add these
```
--stdin-file
/path/to/log/hls-stdin.txt
--stdout-file
/path/to/log/hls-stdout.txt
--stderr-file
/path/to/log/hls-stderr.txt
```

That'll _also_ write to these files, while maintaining the mixed log above. Sometimes it's nice to switch between ways of viewing.
