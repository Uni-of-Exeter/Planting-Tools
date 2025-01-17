# Running the app
1. (Optional but convenient) Open `Planting-Tools.Rproj` with RStudio
2. Go in the folder `ShinyForestry`
3. Download the files from https://universityofexeteruk.sharepoint.com/:f:/r/sites/ADDTREESProject/Data%20Files/JulesOPFilesMissing?csf=1&web=1&e=AhQf5Y
4. Store them in `ShinyForestry/JulesOP/`
5. Copy the 4 files in `ShinyForestry/ElicitorOutput/` to your user Downloads folder
6. Similarly, load the files from https://universityofexeteruk.sharepoint.com/:f:/r/sites/ADDTREESProject/Data%20Files/DownScalingImages?csf=1&web=1&e=RwsXtw into a folder `ShinyForestry/DownScalingImages`.
7. From RStudio, open app.R and click on the `Run App` button, or run the R command `shiny::runApp('ShinyForestry', launch.browser = TRUE)`
8. Wait a few seconds / a minute for the new window to popup

# Instructions for contributors
- Code is meant to be readable by humans, not only computers, so comment your code and use meaningful variable names. Unreadable code will require a lot of time and effort to fix and modify.
- Once you are happy with your branch and want to share your changes with everybody, make a Pull Request to the `dev` branch. Daniel (or Timothée for now) will process it.
- Once in a while, when `dev` is stable enough, `master` will be updated.
- When working, you need to pull changes from `dev` fairly often (especially before pushing to `dev`), because you may have conflicts when you try to push to `dev` if e.g. someone else worked on the same file as you did. Dealing with file conflicts takes time.
To get the latest updates to code, these commands can be very helpful after you setup git access with e.g. SSH keys (save your local changes, pull the latest updates, restore your changes)
```sh
git stash
git pull
git stash pop
```
- Add some details in the commits and Pull Requests (what you changed, what is fixed, the purpose), this is very useful for everyone to understand what you did. This will also let you search more easily in the future.
- Avoid making large updates, small updates are easier to understand.
- Do not store large files (e.g. `.RData` files, media, data) on GitHub, store them instead of Sharepoint / OneDrive.
