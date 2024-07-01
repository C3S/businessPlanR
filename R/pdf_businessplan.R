# Copyright 2024 Meik Michalke <meik.michalke@c3s.cc>
#
# This file is part of the R package businessPlanR.
#
# businessPlanR is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# businessPlanR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with businessPlanR.  If not, see <http://www.gnu.org/licenses/>.

#' R Markdown output format based on \code{bookdown::pdf_document2}
#'
#' This output format definition extends \code{bookdown::pdf_document2} in two ways.
#' Firstly, it allows to manually define directories and files that should be
#' copied or linked to the build directory. Secondly, you can manually set
#' the number of \code{pdflatex} runs.
#' 
#' While working on a more complex business plan including a glossary and
#' an index with keywords, we encountered the problem that \code{rmarkdown::render()}
#' did one run of \code{pdflatex} too few to produce a PDF file with correct page numbers.
#' As a workaround, we had it to render the LaTeX file only, copied missing
#' files manually and ran \code{pdflatex} as often as necessary.
#' 
#' We generalized this workaround into this output format definition for
#' \code{rmarkdown::render()}.
#'
#' @param base_format A function, the base format to extend.
#' @param link_dirs Vector of directories to symlink to the build directory, in absolute paths.
#' @param copy_files Vector of files to copy to the build directory, in absolute paths.
#' @param makeindex Logical, whether to invoke makeindex after pdflatex.
#' @param makeglossary Logical, whether to invoke makeglossary after pdflatex.
#' @param pdflatex_runs Either an integer number, the number of times pdflatex (and makeindex/makeglossary, respectively)
#'    should run to render the document.
#' @param ... Further options to pass to \code{base_format}.
#' @return An R Markdown output format object to be passed to \code{rmarkdown::render()}.
#' @importFrom bookdown pdf_document2
#' @export

pdf_businessplan <- function(
    base_format = bookdown::pdf_document2
  , link_dirs = c()
  , copy_files = c()
  , makeindex = FALSE
  , makeglossary = FALSE
  , pdflatex_runs = "auto"
  , keep_tex = TRUE
  , ...
){
  output <- base_format(...)

  intermediates_generator_orig <- output[["intermediates_generator"]]

  output[["intermediates_generator"]] <- function(
      link_dirs = link_dirs
    , copy_files = copy_files
    , ...
  ){
    im <- intermediates_generator_orig(...)
    if(length(link_dirs) > 0){
      im <- c(im, link_dirs)
    } else {}
    if(length(copy_files) > 0){
      im <- c(im, copy_files)
    } else {}
    return(im)
  }

  if(any(isTRUE(makeindex), isTRUE(makeglossary), !identical(pdflatex_runs, "auto"))){
    output[["pandoc"]][["keep_tex"]] <- TRUE
  } else {}

  return(output)
}
