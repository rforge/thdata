[B
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

.libPaths("")
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
            deps <- c(deps[!(deps %in% rownames(ipkg))], name)
            if (length(deps) > 0)
                install.packages(deps,
                                 repos = "http://CRAN.at.R-project.org", lib = libdir,
                                 dependencies = TRUE)
            update.packages(lib.loc = libdir, ask = FALSE, 
                            repos = "http://CRAN.at.R-project.org", checkBuilt = TRUE)
        }
        oldversion <- packageDescription(name, lib.loc = libdir, fields = "Version")
print(oldversion)
print(version)
        stopifnot(compareVersion(oldversion, version) == -1)
        return(TRUE)
    }

    .build <- function() {
        system(paste("export R_LIBS=", libdir, 
                     "/; R CMD build ", dir, sep = ""))
        system(paste("cp ", file, chkdir))
        return(TRUE)
    }

    .check <- function(file, dir = chkdir) {
        wd <- setwd(dir)
        version <- packageDescription(name, lib.loc = libdir, fields = "Version")
        system(paste("export R_LIBS=", libdir, "/; ", Rbin, " CMD check --as-cran ",  file, " >>", 
                     paste(file, ".checklog_", version, sep = ""), sep = ""))
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

    .split_dependencies <- function(x) {
        .split2 <- function(x) {
            x <- sub("[[:space:]]+$", "", x)
            x <- unique(sub("^[[:space:]]*(.*)", "\\1", x))
            names(x) <- sub("^([[:alnum:].]+).*$", "\\1", x)
            x <- x[names(x) != "R"]
            x <- x[nzchar(x)]
            x <- x[!duplicated(names(x))]
            lapply(x, tools:::.split_op_version)
        }
        if (!any(nzchar(x))) 
            return(list())
        sapply(unlist(lapply(strsplit(x, ","), .split2), FALSE, FALSE), function(x) x$name)
    }

        pkgs <- available.packages(contriburl = contrib.url("http://cran.at.r-project.org"))
        pkgs <- pkgs[, c("Depends", "Imports", "Suggests")]
        x <- do.call("paste", as.data.frame(pkgs))
        odeps <- rownames(pkgs)[grep(name, x)]
        adeps <- unlist(lapply(odeps, function(o) {
            s <- pkgs[o, "Suggests"]
            if (is.na(s)) return(NA)
            .split_dependencies(s)
        }))
        deps <- c(odeps, adeps[!is.na(adeps)])
        
        ipkg <- installed.packages(lib.loc = libdir)
        deps <- deps[!(deps %in% rownames(ipkg))]
        if (length(deps) > 0) {
            install.packages(odeps,
                             repos = "http://CRAN.at.R-project.org", lib = libdir, 
                             dest = revdir, dependencies = TRUE)
        }        

        update.packages(lib.loc = libdir, ask = FALSE,
                        repos = "http://CRAN.at.R-project.org", dest = revdir, checkBuilt = TRUE)

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
        system(paste("tar -xzvf", file))
        if (removeRoutsave) 
            writeLines(c("Rout.save", "tests/Examples"), 
                       con = file.path(name, ".Rbuildignore"))
        system(paste("export R_LIBS=", libdir, "/; ", 
            Rbin, " CMD build --compact-vignettes=\"both\" --resave-data ", 
                     name, sep = ""))
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

make_diff <- function(revdir) {

    pkgs <- list.files(path = revdir, pattern = "gz$")

    for (p in pkgs) {
        f <- list.files(pattern = paste(p, ".checklog", sep = ""), 
                        path = revdir, full.names = TRUE)
        d <- Rdiff(f[1], f[2])
        if (!isTRUE(all.equal(d, 0L))) {
            cat(p)
            print(d)
            cat("\n\n")
        }
    }
}
