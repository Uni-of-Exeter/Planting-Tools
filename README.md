# Planting-Tools project

## Instructions for contributors
- Once you are happy with your branch and want to share your changes with everybody, make a Pull Request to the `dev` branch. Daniel (or Timothée for now) will process it.
- Once in a while, when `dev` is stable enough, `master` will be updated.
- When working, you need to pull changes from `dev` fairly often (especially before pushing to `dev`), because you may have conflicts when you try to push to `dev` if e.g. someone else worked on the same file as you did. Dealing with file conflicts takes time.
- Add some details in the commits and Pull Requests (what you changed, what is fixed, the purpose), this is very useful for everyone to understand what you did. This will also let you search more easily in the future.
- Avoid making large updates, small updates are easier to understand.

## Running the app
1. (Optional but convenient) Open `Planting-Tools.Rproj` with RStudio
2. Go in the folder `ShinyForestry`
3. Download the files from https://universityofexeteruk.sharepoint.com/:f:/r/sites/ADDTREESProject/Data%20Files/JulesOPFilesMissing?csf=1&web=1&e=AhQf5Y
4. Store them in `ShinyForestry/JulesOP/`
5. Copy the 4 files in `ShinyForestry/ElicitorOutput/` to your user Downloads folder
6. Similarly, load the files from https://universityofexeteruk.sharepoint.com/:f:/r/sites/ADDTREESProject/Data%20Files/DownScalingImages?csf=1&web=1&e=RwsXtw into a folder `ShinyForestry/DownScalingImages`.
7. From RStudio, open app.R and click on the `Run App` button, or run the R command `shiny::runApp('ShinyForestry', launch.browser = TRUE)`
8. Wait a few seconds / a minute for the new window to popup
