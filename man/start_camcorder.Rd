% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataviz.R
\name{start_camcorder}
\alias{start_camcorder}
\title{Main color object}
\usage{
start_camcorder(dir = NULL, width = NULL, height = NULL)
}
\arguments{
\item{dir}{A string representing a valid directory on the machine for which
the camcorder will save rendered plots}

\item{width}{An integer for the width in cm of the rendered and saved plot}

\item{height}{An integer for the height in cm of the rendered and saved plot}
}
\value{
Nothing
}
\description{
`start_camcorder()` begins a recording of the "Viewer" tab, saving copies
of the rendered plot in the directory of your choosing. It uses defaults
for the device, units, and dpi. To change those, use `camcorder::gg_record()`
instead of this function. It's designed to be used in conjunction with
`theme_adamb_cam()`. Execute `camcorder::gg_stop_recording()` to end the
recording.
}
\examples{
# Pass in desired directory and dimensions
start_camcorder("/my_directory", 16, 9)
}
\seealso{
[theme_adamb_cam()]
}
