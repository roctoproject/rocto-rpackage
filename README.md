<p align="center">
  <img src="TBD" width="20%"></img>
  <h1 align="center">rocto</h1>
  <h4 align="center">Perform computation on a worldwide distributed cluster</h4>
  <p align="center">
    <a href="https://travis-ci.org/roctoproject/rocto-rpackage"><img src="https://travis-ci.org/roctoproject/rocto-rpackage.svg?branch=master"></a>
    </a>
  </p>
</p>
<br/>

### what?
This package allows you to easily create computational workloads in `R` to send to the rocto distributed computing cluster, a free and easy-to-use alternative to expensive and complex high-performance cluster computers.

### when?
Not yet, but soon.

### how?
More extensive documentation is on its way. For now, you can play with these steps:

1. Install the [rocto desktop client](https://github.com/roctoproject/rocto-client/releases).

2. Install the package `devtools::install_github("roctoproject/rocto-rpackage")`

3. Load the package `library(rocto)`

4. Create a new `rocto` job by using `roctoNew("myJobName")`.

5. Create your job using the `main.R` and `params.R` files.

6. Check and pack your job using `roctoPack("path/to/myJobName")`.

7. Open the `.rocto` file in the desktop client and send it away!
