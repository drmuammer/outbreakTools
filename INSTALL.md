# outbreakTools — Installation Guide

This module ships as **R source code**, not a pre-compiled `.jmo`. A single
`.jmo` file cannot run on more than one operating system because it carries
platform-specific compiled libraries (`.so` on Linux/macOS, `.dll` on
Windows). To use the module on your machine, build it locally **once** per
platform.

If you only want to **use** the module (not develop it), the cleanest path is
to publish it to the [jamovi library](https://library.jamovi.org/) — the
jamovi build farm produces binaries for Windows, macOS Intel, macOS ARM, and
Linux automatically. End users then install from the in-app library with one
click. See section **4** below.

---

## 1. Prerequisites — all platforms

You need three things:

| Tool | Version | Notes |
|---|---|---|
| **R** | 4.3.x or later (4.5.x recommended) | https://cran.r-project.org |
| **jamovi** | 2.4 or later | https://www.jamovi.org/download.html |
| **A C/C++ toolchain** | platform-specific (see below) | needed to compile native R packages (sf, lubridate, epiR) |

You do **not** need RStudio. The plain `R` console (RGui on Windows,
`R.app` on macOS, `R` in a terminal on Linux) is sufficient.

---

## 2. Platform-specific toolchain

### Windows

1. Install R from https://cran.r-project.org/bin/windows/base/
2. Install **Rtools** matching your R version from
   https://cran.r-project.org/bin/windows/Rtools/
   — Rtools45 for R 4.5.x and 4.6.x, Rtools44 for R 4.4.x, Rtools43 for R 4.3.x.
3. Open R (RGui) and verify the toolchain is found:
   ```r
   Sys.which("make")
   # Should return something like "C:\\rtools45\\usr\\bin\\make.exe"
   ```
   An empty return means Rtools is not on `PATH` — re-run the installer with
   defaults.

> ⚠️ **Avoid paths with spaces or non-ASCII characters** when cloning this
> repo on Windows. `C:\jamovi-dev\outbreakTools` works; `C:\Users\xxx\Yeni
> Klasör\` may break the build.

### macOS

1. Install R from https://cran.r-project.org/bin/macosx/
2. Install Xcode Command Line Tools:
   ```bash
   xcode-select --install
   ```
3. Install [gfortran](https://mac.r-project.org/tools/) (needed by some R
   packages with Fortran code). The macOS R installer page lists the
   recommended version for each macOS release.

### Linux (Ubuntu / Debian)

```bash
sudo apt-get update
sudo apt-get install -y r-base r-base-dev \
    libcurl4-openssl-dev libssl-dev libxml2-dev libgdal-dev \
    libudunits2-dev libproj-dev
```

For other distributions, install the equivalent of `r-base-dev` plus
development headers for GDAL, PROJ, UDUNITS, libcurl, libssl, libxml2.

---

## 3. Build the module locally

### Step 3a. Install jmvtools (once)

In R:

```r
install.packages('jmvtools',
  repos = c('https://repo.jamovi.org', 'https://cran.r-project.org'))
```

### Step 3b. Install required CRAN dependencies (once)

This step is optional — `jmvtools::install()` pulls dependencies
automatically — but doing it up-front gives you cleaner error messages if
something fails:

```r
install.packages(c(
  "R6", "dplyr", "tidyr", "ggplot2", "lubridate", "janitor",
  "stringr", "stringi", "scales", "epitools", "epiR", "purrr"
))
```

`epiR` depends on `sf`, which needs the GDAL/PROJ system libraries listed
above for Linux. On Windows and macOS, CRAN provides pre-built binaries so
no system libraries are needed.

### Step 3c. Tell jmvtools where jamovi lives

```r
library(jmvtools)
jmvtools::check()                   # auto-detect
# If auto-detect fails:
jmvtools::check(home = "C:/Program Files/jamovi 2.7.30.0")           # Windows
jmvtools::check(home = "/Applications/jamovi.app/Contents/Frameworks/jamovi") # macOS
jmvtools::check(home = "/usr/lib/jamovi")                                     # Linux
```

The exact path depends on how you installed jamovi.

### Step 3d. Build and install the module

```r
setwd("/path/to/outbreakTools")     # the folder containing DESCRIPTION
jmvtools::install()
```

You should see `Module installed successfully`. Close and reopen jamovi —
**Outbreak Tools** will appear in the Analyses ribbon with eight analyses.

### Step 3e. (Optional) Produce a `.jmo` for sharing

To share a pre-built `.jmo` with colleagues **on the same operating system**:

```r
jmvtools::create("outbreakTools_1.1.0_<platform>.jmo")
```

Replace `<platform>` with `win64`, `macos`, or `linux` so recipients know
which OS the file targets. Recipients install via **jamovi → modules menu
(⊕) → Sideload → choose the .jmo file**.

> ⚠️ A `.jmo` built on macOS will **not** run on Windows, and vice versa.
> Build separately for each target OS.

---

## 4. Recommended path: publish to the jamovi library

Instead of building per-platform `.jmo`s yourself, submit the module to the
official library:

```r
jmvtools::publish()
```

The jamovi build farm then compiles and hosts binaries for all supported
platforms. Users install with one click from **jamovi → Modules → jamovi
library**. Updates propagate automatically.

See the [jamovi developer hub](https://dev.jamovi.org/) for the submission
process and review guidelines.

---

## 5. Troubleshooting

### `This analysis has terminated, likely due to hitting a resource limit`

Despite the message text, this is almost never a memory issue — it means the
R back-end crashed while initialising the analysis. Common causes:

| Symptom | Cause | Fix |
|---|---|---|
| All eight analyses show this message | Module compiled for wrong OS | Re-run `jmvtools::install()` on the target machine |
| Only one analysis affected | Missing CRAN dependency, or yaml/R mismatch | Check jamovi's log file (see below) |
| Module appears in menu but analyses don't run | Stale install cache | jamovi → Modules → Outbreak Tools → Remove, then reinstall |

### Reading the actual error

To see the real R error behind the friendly message, launch jamovi from a
terminal so it writes errors to stdout:

**Windows** — Command Prompt:
```
"C:\Program Files\jamovi 2.7.30.0\bin\jamovi.exe"
```

**macOS** — Terminal:
```bash
/Applications/jamovi.app/Contents/MacOS/jamovi
```

**Linux**:
```bash
jamovi
```

Then run the failing analysis. The R traceback appears in the terminal.

### `package 'sf' could not be loaded` on Linux

Install the system libraries listed in section 2 (Linux) and reinstall `sf`:

```r
install.packages("sf", type = "source")
```

### Module installs but Outbreak Tools menu is empty

Make sure jamovi was fully closed (including any background process) before
reopening. On Windows: check Task Manager for `jamovi.exe` and end any
lingering instance.

---

## 6. Module structure (for developers)

```
outbreakTools/
├── DESCRIPTION          R package metadata
├── NAMESPACE            Exports and imports
├── R/                   R source code (one .b.R + .h.R per analysis)
│   ├── obtclean.b.R     ← data quality & cleaning logic
│   ├── obtclean.h.R     ← auto-generated by jmvtools
│   ├── obtdescriptive.b.R
│   ├── obtepicurve.b.R
│   ├── obtcohort.b.R
│   ├── obtcasecontrol.b.R
│   ├── obtstratified.b.R
│   ├── obtlogistic.b.R
│   ├── obtsamplesize.b.R
│   └── utils.R          ← shared helpers (incl. fingerprint clustering)
├── jamovi/              Analysis definitions
│   ├── 0000.yaml        ← module manifest
│   ├── *.a.yaml         ← option definitions
│   ├── *.r.yaml         ← result definitions
│   └── *.u.yaml         ← UI layout
└── inst/extdata/        Example dataset (foodborne_outbreak.csv)
```

After editing any `.a.yaml`, `.r.yaml`, or `.u.yaml`, re-run
`jmvtools::install()` so the corresponding `.h.R` files are regenerated.

---

*outbreakTools — Türkiye FETP, 2025*
