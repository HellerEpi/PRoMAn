# PRoMAn
PRoMAn is an R package that grew out of personal tools.  In its current state, it can help with file organization and data management, rudimentary workflows, project organization and logging of work, saving fallback statuses of projects, and contains a system for helping with the creation of scientific products, which I call "Blitzschreiben."

The following notes are from the development notes (which can be found at PRoMAn/dev/notes.txt)

-------------------------------------------------------------------------------------------------
					     NOTES
-------------------------------------------------------------------------------------------------
- 0.6.0.9002 - 02.12.2026 > fixed the bugs with the figures registry and save_figure()

- 0.6.0.9001 - 04.12.2025 > "PRoMAn" Beta
	> added in function for reference management to make notes as reading, rather than make a 	full annotated bibliography all at once for a much more natural workflow
		- still extracts all the information for citation in note and updates .proman
		- can combine and review notes as well
		- included in update_bibliography()

- 0.6.0.9000 - 06.11.2025 > "PRoMAn" Beta
	> added in the reference management functions and further tested blitzschreiben
	> now can create a bibliography, update it with an annotation function.
	> released as beta on GitHub
	> really probably don't need any other core functionality added before first true version

 - 0.5.0.9016 - 20.10.25
	> consolidated all pathing and simplified calls to files and folders
	> now just read or select files takes care of pretty much everything.  E.g.:
			dc_data <- read_file("DC_counts", "cleaned") #where DC_counts is a 					filename and cleaned is the folder .../data/cleaned

- 0.4.0.9000 - 06.10.25
	> implemented the fallback system, user initiated backup saves of all files in the 	project in a hidden folder.  
	> does not do versioning or multiple saves, only ein fallback save at a time.  Clean, 	minimal, does not try to do what already Git does.
	> Users can call the function fallback() to reset to the previous fallback status.
	> last fallback set should be reflected in project logs.


- 0.3.0.9000 - 03.10.25 - added in a workflows system.  PRoMan *should* now be able to recognize 	marked chunks of code, process them as workflows, and apply them to sets of files or save 	them as scripts to be sourced.
	> This may be the last core feature added in order to prevent feature creep. Helper 	functions and improvements of current core capabilities should probably be the focus 	moving forward.
		~ E.g., functions that help manage sources may need expanding and working with, 		everything needs testing and streamlining, etc.
		~ the only other potential novel addition would be an emergency backup/snapshot 		system.  A quick rip-cord emergency reset that stops some goof ups so could save 		a lot of time.  "Fallback"? Would have to be easier & lighter than GitHub.


- 0.2.0.9000 - including a hidden .proman file in projects so that there are no setwd() 	conflicts, functions have meta-data accessible, some things are streamlined.


## Current Status
PRoMAn is under development, and the current release (0.6.0.9002) is a test release.  Major components are functioning, but there are still some bugs and aesthetic issues, and there may be differences in final functionality.

## Installation

### Method 1: Download Release
1. Go to [Releases](https://github.com/HellerEpi/PRoMAn/releases)
2. Download the latest version
3. Extract the files
4. install using devtools

### Method 2: Clone Repository
```bash
git clone https://github.com/HellerEpi/PRoMAn.git
cd PRoMAn


