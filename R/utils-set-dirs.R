#' Set and return all model directories for the project
#'
#' @details
#' Sensitivity groups will have the base model directory prepended.
#' If directories do not exist, they will be assigned `NULL` and a warning
#' issued. If the `models_dir` does not exist, an error will be thrown.
#'
#' @param models_dir Full path in which the SS3 model directories are
#' located for the current assessment year
#' @param base_models_dir Name of the base models directory
#' @param sens_models_dir Name of the sensitivity models directory
#' @param request_models_dir Name of the request models directory
#' @param test_models_dir Name of the test models directory
#' @param base_models_dirs A vector of subdirectory names in
#' `base_models_dir` that each contain an individual SS3 base model
#' @param sens_models_dirs A vector of subdirectory names in `sens_models_dir`
#' that each contain an individual SS3 sensitivity model
#' @param request_models_dirs A vector of subdirectory names in
#' `request_models_dir` that each contain an individual SS3 base model request
#' model
#' @param test_models_dirs A vector of subdirectory names in `test_models_dir`
#'  that each contain an individual SS3 base model test model
#' @param suppress_warnings If `TRUE`, warnings about directories not existing
#' will not be shown
#'
#' @return A list of twelve vectors of full paths, which will have `NA`
#' elements for those which do not exist:
#' 1.  Full path of `models_dir`
#' 2.  Full path of `base_model_dir`
#' 3.  Full path of `sens_models_dir`
#' 4.  Full path of `request_models_dir`
#' 5.  Full path of `test_models_dir`
#' 6.  A vector of the base model directories
#' 7. A vector of the sensitivity model directories
#' 8. A vector of the request model directories
#' 9. A vector of the test model directories
#'
#' @export
set_dirs <- \(models_dir = NA,
              base_models_dir = "01-base-models",
              sens_models_dir = "02-sensitivity-models",
              request_models_dir = "03-request-models",
              test_models_dir = "03-test-models",
              base_models_dirs = NA,
              sens_models_dirs = NA,
              request_models_dirs = NA,
              test_models_dirs = NA,
              suppress_warnings = FALSE){

  if(is.null(models_dir) || is.na(models_dir)){
    bail("`models_dir` must not be `NULL` or `NA`")
  }

  if(!dir.exists(models_dir)){
    bail("`models_dir` does not exist")
  }

  if(is.null(base_models_dir) ||
     is.null(sens_models_dir) ||
     is.null(request_models_dir) ||
     is.null(test_models_dir) ||
     is.null(base_models_dirs) ||
     is.null(sens_models_dirs) ||
     is.null(request_models_dirs) ||
     is.null(test_models_dirs)){
    bail("None of the following directory names can be NULL:\n",
         "(If you want them to be ignored, set them to NA)\n",
         "`base_models_dir`\n`sens_models_dir`\n",
         "`request_models_dir`\n`test_models_dir`\n`base_models_dirs`\n",
         "`sens_models_dirs`\n`request_models_dirs`\n",
         "`test_models_dirs`\n")
  }

  root_dirs_rel <- c(base_models_dir,
                     sens_models_dir,
                     request_models_dir,
                     test_models_dir)
  root_dirs <- file.path(models_dir, root_dirs_rel)

  # This works if any of `root_dirs_rel` are `NA` so there is no explicit check
  # for `NA` here
  root_dirs_exist <- map_lgl(root_dirs, ~{
    dir.exists(.x)
  })

  if(!suppress_warnings){
    if(!all(root_dirs_exist)){
      if(sum(root_dirs_exist) == length(root_dirs_exist) - 1){
        warning("The following directory does not exist:\n",
                root_dirs[!root_dirs_exist], "\n")
      }else{
        warning("The following directories do not exist:\n",
                paste(root_dirs[!root_dirs_exist], collapse = "\n"), "\n")
      }
    }
  }

  subdirs_rel <- list(base_models_dirs,
                      sens_models_dirs,
                      request_models_dirs,
                      test_models_dirs)

  has_models_subdirs <- map(subdirs_rel, \(type){
    map(type, function(group){
      map_lgl(group, ~{!is.na(.x)})
    })
  })

  # Make full paths. Some may contain NA
  subdirs <- map2(root_dirs, subdirs_rel, \(root_dir, subdir){
    map(subdir, \(group){
      file.path(root_dir, group)
    })
  })

  dirs <- map(subdirs, \(subdir){
    map(subdir, \(group){
      map_chr(group, \(dr){
        if(dir.exists(dr)){
          return(dr)
        }
        NA_character_
      })
    })
  })

  # Prepend the base model to each of the sensitivity model groups
  dirs[[2]] <- map(dirs[[2]], \(sns){
    if(is.na(sns[1])){
      NA
    }else{
      c(dirs[[1]][[1]], sns)
    }
  })

  # Prepend the base model to each of the test model groups
  dirs[[4]] <- map(dirs[[4]], \(sns){
    if(is.na(sns[1])){
      NA
    }else{
      c(dirs[[1]][[1]], sns)
    }
  })

  # Remove `NA` entries
  dirs <- dirs |>
    map(~{
      if(is.na(.x[1])){
        return(NA)
      }
      .x
    })

  list(models_dir = models_dir,
       base_model_dir = base_models_dir,
       sens_models_dir = sens_models_dir,
       request_models_dir = request_models_dir,
       test_models_dir = test_models_dir,
       base_models_dirs = dirs[[1]],
       sens_models_dirs = dirs[[2]],
       request_models_dirs = dirs[[3]],
       test_models_dirs = dirs[[4]])
}
