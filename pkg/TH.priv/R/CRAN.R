
.pkg_description <- function(dir)
    tools:::.read_description(file.path(dir, "DESCRIPTION"))

.pkg_name <- function(dir)
    .pkg_description(dir)[["Package"]]

.pkg_version <- function(dir)
    .pkg_description(dir)[["Version"]]

.pkg_dep <- function(dir) {
    db <- .pkg_description(dir)
    names(c(tools:::.get_requires_with_version_from_package_db(db, "Depends"),
            tools:::.get_requires_with_version_from_package_db(db, "Imports"),
            tools:::.get_requires_with_version_from_package_db(db, "Suggests")))
}

target <- "~/Rcheck"
Rbin <- "/usr/src/R-devel/bin/R"

make_check <- function(dir, revs = FALSE, removeRoutsave = FALSE) {

print(dir)

    name <- .pkg_name(dir)
    version <- .pkg_version(dir)
    file <- ret <- paste(name, "_", version, ".tar.gz", sep = "")
    deps <- c(name, .pkg_dep(dir))
    chkdir <- file.path(target, name)
    libdir <- file.path(target, name, "lib")
    revdir <- file.path(chkdir, "rev")

    .prepare_check <- function() {
        if (!file.exists(target))
            dir.create(target)
        if (!file.exists(chkdir))
            dir.create(chkdir)
        if (!file.exists(libdir)) {
            dir.create(libdir)
            install.packages(deps, 
                             repos = "http://CRAN.at.R-project.org", lib = libdir,
                             dependencies = TRUE)
        } else {
            ipkg <- installed.packages(lib.loc = libdir)
            deps <- deps[!(deps %in% rownames(ipkg))]
            if (length(deps) > 0)
                install.packages(deps,
                                 repos = "http://CRAN.at.R-project.org", lib = libdir,
                                 dependencies = TRUE)
            update.packages(lib.loc = libdir, ask = FALSE, 
                            repos = "http://CRAN.at.R-project.org")
        }
        oldversion <- packageDescription(name, lib.loc = libdir, fields = "Version")
        stopifnot(compareVersion(oldversion, version) == -1)
        return(TRUE)
    }

    .build <- function() {
        system(paste("export R_LIBS=", libdir, sep = ""))
        system(paste("R CMD build", dir))
        system(paste("cp ", file, chkdir))
        return(TRUE)
    }

    .check <- function(file, dir = chkdir) {
        wd <- setwd(dir)
        version <- packageDescription(name, lib.loc = libdir, fields = "Version")
        system(paste("export R_LIBS=", libdir, sep = ""))
        system(paste(Rbin, " CMD check --as-cran",  file, " >>", 
                     paste(file, ".checklog_", version, sep = "")))
        setwd(wd)
        return(TRUE)
    }

    .install <- function() {
        wd <- setwd(chkdir)
        system(paste(Rbin, " CMD INSTALL -l", libdir, file))
        setwd(wd)
        return(TRUE)
    }

    .rev_dep <- function() {

        if (!file.exists(revdir))
            dir.create(revdir)

        pkgs <- available.packages(contriburl = contrib.url("http://cran.at.r-project.org"))
        pkgs <- pkgs[, c("Depends", "Imports", "Suggests", "Enhances")]
        x <- do.call("paste", as.data.frame(pkgs))
        deps <- odeps <- rownames(pkgs)[grep(name, x)]

        ipkg <- installed.packages(lib.loc = libdir)
        deps <- deps[!(deps %in% rownames(ipkg))]
        if (length(deps) > 0)
            install.packages(deps,
                             repos = "http://CRAN.at.R-project.org", lib = libdir, 
                             dest = revdir, dependencies = TRUE)

        update.packages(lib.loc = libdir, ask = FALSE,
                        repos = "http://CRAN.at.R-project.org", dest = revdir)

        wd <- setwd(revdir)
        pkgs <- sapply(strsplit(f <- list.files(pattern = "tar\\.gz"), "_"), function(x) x[1])
        pos <- match(odeps, pkgs)
        if (any(is.na(pos)))
            download.packages(odeps[is.na(pos)], 
                destdir = revdir, repos = "http://CRAN.at.R-project.org")
        pos <- match(pkgs, odeps)
        out <- sapply(f[is.na(pos)], function(x) file.remove(x))
        setwd(wd)
        return(TRUE)
    }

    .build_CRAN <- function() {
        wd <- setwd(chkdir)
        system(paste("export R_LIBS=", libdir, sep = ""))
        system(paste("tar -xzvf", file))
        if (removeRoutsave) 
            writeLines(c("Rout.save", "Examples"), 
                       con = file.path(name, ".Rbuildignore"))
        system(paste(Rbin, " CMD build --compact-vignettes=\"both\" --resave-data", 
                     name))
        setwd(wd)
        return(TRUE)
     }

     stopifnot(.prepare_check())
     stopifnot(.build())
     stopifnot(.check(file))
     if (revs) rd <- .rev_dep()

     if (revs && rd) {
         files <- list.files(path = revdir)
         print(mclapply(files, .check, dir = revdir))
     }

     stopifnot(.install())

     if (revs && rd) {
         print(mclapply(files, .check, dir = revdir))
     }

     stopifnot(.build_CRAN())
}
