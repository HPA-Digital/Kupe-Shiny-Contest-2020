* Author Sarah R

* For every computer with R-studio:

Under Tools set your global options to 
	turnoff Restore.RData
and	NEVER save workspace

This can also be done for individual projects


We will all use the R drive library. Check default library location
	.libPaths()
When the project 
	health-lifestyle-survey.Rproj
is opened from
	R:\R\DataExplorer\health-lifestyle-survey
this opens r-studio and runs the .Rprofile code first which sets the default library to R drive.

# PE: set the location of the library on the HPA network as the first place to install and load R packages
# from, if it can be found (while leaving the other locations on your c: drive still in use if the R 
# drive is not available):
.libPaths(c("R:/R/R packages", .libPaths()))

The default path is the shared packages library
.libPaths()
[1] "R:/R/R packages"      "\\\\App02/SarahR$/My Documents/R/win-library/3.5"
[3] "C:/Program Files/R/R-3.5.0/library"              



*HELP FILES:
If you open a .md file in Rstudio it has a “preview” button which will show it for you in the Viewer pane (which is one of the tabs in the pane with Files, Plots, etc).  Once its in the preview, you can also click on the little “show in new window” icon above the preview which will open it in a browser.










