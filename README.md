Collecting data for the warmup curve charts
======================================================

1) Follow the steps in `talkyard/README.md` to get the application working.

2) Import the benchmark reference site into the application:

    1) First, enable site importing by adding `talkyard.mayImportSite=true` to `talkyard/conf/app-dev.conf` and restart the application to make sure the changes have been picked up.
    2) Executing `curl -sS -X POST -H "Content-Type: application/json" -d @benchmark-site.json  http://localhost/-/import-site-json` while the application is running.
    3) Search the output of the previous command and take note of the automatically generated name of the imported site (e.g. `http://site--11.localhost`).

3) Build our fork of `wrk` by running `make` in the `wrk` folder.

4) `cd` into the `talkyard` application folder and run `sudo ../capture-warmup-curves $SITE $OUTDIR` where `$SITE` is your site name from step 2 and `$OUTDIR` is the folder in which you would like to store the results. NB: Running `../capture-warmup-curves` will reset the working tree of the current Git project (presumably `talkyard`) as it iterates over the different configurations.

5) To generate the charts, `cd` into the `charting` folder and run `stack run $OUTDIR`. The `.svg` files will be placed in `$OUTDIR`.
