Collecting data for the warmup curve charts
======================================================

1) Clone the `benchmarking` branch from the following fork of `talkyard`.

```
git clone https://github.com/jirkamarsik/talkyard --branch benchmarking
```

1) Follow the steps in `talkyard/README.md` to get the application working.

1) Import the benchmark reference site into the application:

    1) First, enable site importing by adding `talkyard.mayImportSite=true` to `talkyard/conf/app-dev.conf` and restart the application to make sure the changes have been picked up.
    1) Executing `curl -sS -X POST -H "Content-Type: application/json" -d @benchmark-site.json  http://localhost/-/import-site-json` while the application is running.
    1) Search the output of the previous command and take note of the automatically generated name of the imported site (e.g. `http://site--11.localhost`).

1) Build our fork of `wrk` by running `make` in the `wrk` folder.

1) Check `graalvmce.patch` and `graalvmee.patch`. These patch files describe how the Dockerfile for the application's Docker image are to be modified so that they use GraalVM CE/EE. You can modify these to test on different versions of GraalVM. Some might use `curl` to download publicly available distributions (such as GraalVM CE) or use the `COPY` command to search for GraalVM on your local filesystem. The latter will require you to copy the GraalVM distribution tarfile into `talkyard/images/app` under the name that is referenced as `GRAALVM_TARFILE` in the patch.

1) `cd` into the `talkyard` application folder and run `sudo ../capture-warmup-curves $SITE $OUTDIR` where `$SITE` is your site name from step 3 and `$OUTDIR` is the folder in which you would like to store the results. (`sudo` is required by several `docker` commands used by the script. Calling the whole command with `sudo` avoids the script from repeatedly prompting you for your password.) NB: Running `../capture-warmup-curves` will reset the working tree of the current Git project (presumably `talkyard`) as it iterates over the different configurations.

1) To generate the charts, `cd` into the `charting` folder and run `stack run $OUTDIR`. The `.svg` files will be placed in `$OUTDIR`. NB: Before you run this command, you might want to change the owner/group of `$OUTDIR` as it will likely belong to `root`, since `capture-warmup-curves` was called with `sudo`.
