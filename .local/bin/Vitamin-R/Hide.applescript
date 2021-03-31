
on run (argv)
	set argn to count of argv
	set appName to "Vitamin-R 3"
	
	tell application "Finder"
		set currentScriptPath to (POSIX path of (container of (path to me) as alias))
	end tell
	
	set commonScript to (load script currentScriptPath & "common.scpt")
	
	tell application "System Events"
		set frontApp to first application process whose frontmost is true
		set frontAppName to name of frontApp
		if frontAppName = appName then
			set mainWindow to front window of frontApp
			tell mainWindow
				hideWindow(mainWindow) of commonScript
			end tell
		end if
	end tell
end run