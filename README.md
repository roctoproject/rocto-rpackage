# rocto [![travis-ci](https://travis-ci.org/roctoproject/rocto-rpackage.svg?branch=master)](https://travis-ci.org/roctoproject/rocto-rpackage)
Perform computation on a worldwide distributed cluster

### what?
This package allows you to create jobs to send to the rocto distributed computing cluster, a free and easy-to-use alternative to expensive and complex high-performance cluster computers.

### when?
Not yet, but soon.

### how?
More extensive documentation is on its way. For now, you can play with these steps:

0. Install the [rocto desktop client](https://github.com/roctoproject/rocto-client/releases).

1. Install and load the package `devtools::install_github("roctoproject/rocto-rpackage")`

2. Create a new `rocto` job by using `newJob("myJobName")`.

3. Create your job using the `main.R` and `params.R` files.

4. Check and pack your job using `packJob("path/to/myJobName")`.

5. Open the `.rocto` file in the desktop client and send it away!
