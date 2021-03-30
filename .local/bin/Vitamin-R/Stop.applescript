
on run (argv)
	set argn to count of argv
	set appName to "Vitamin-R 3"
	
	tell application "Finder"
		set currentScriptPath to (POSIX path of (container of (path to me) as alias))
	end tell
	
	set commonScript to (load script currentScriptPath & "common.scpt")
	
	tell application appName
		activate
	end tell
	
	tell application "System Events"
		tell process appName
			set frontmost to true
			set mainWindow to front window
			
			tell mainWindow
				set startTimeSliceBtnName to "Start Time Slice"
				set logBtnName to "Log"
				set startTimedBreakBtnName to "Start Timed Break"
				set finishBtnName to "Finish" -- slice or break finish
				set hideBtnName to "Hide"
				
				set currentStep to ""
				set isRunning to false
				set isStoped to false
				
				set currentStep to getCurrentStep(mainWindow) of commonScript
				
				repeat while isStoped is not true
					set currentStep to getCurrentStep(mainWindow) of commonScript
					log currentStep
					
					if currentStep = "Time Slice Paused." then
						click button "Resume" of mainWindow
						set frontmost to true
					else if currentStep = "Rate Time Slice" then
						set feltState to "My focus was good"
						if argn > 1 then
							set feltState to item 2 of argv
						end if
						
						log feltState
						tell pop up button 1
							click
							delay 0.1
							click menu item feltState of menu 1
						end tell
						
						click button logBtnName
					else if currentStep = "Prepare Timed Break" then
						set myBreakCount to "1"
						if argn > 2 then
							set myBreakCount to item 3 of argv
						end if
						click radio button breakBtnNameWithPomoCount(myBreakCount) of commonScript
						click button startTimedBreakBtnName
					else if currentStep = "Time Slice Running..." or currentStep = "On a Timed Break..." then
						set isRunning to true
					else if currentStep = "Define Time Slice" then
						set isStoped to true
					end if
					
					if isRunning or isStoped then
						hideWindow(mainWindow) of commonScript
						return "OK"
					end if
				end repeat
			end tell
		end tell
	end tell
end run