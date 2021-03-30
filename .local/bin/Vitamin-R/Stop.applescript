
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
				-- set frontmost to true
				-- set startTimeSliceBtn to button "Start Time Slice"
				set startTimeSliceBtnName to "Start Time Slice"
				set logBtnName to "Log"
				set startTimedBreakBtnName to "Start Timed Break"
				set finishBtnName to "Finish" -- slice or break finish
				set hideBtnName to "Hide"
				-- set logAsCompletedCheckBoxName to  "Log as completed"
				
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
						-- set currentStep to getCurrentStep(mainWindow) of commonScript
					else if currentStep = "Rate Time Slice" then
						set feelState to value of pop up button 1
						log feelState
						if isRunning then
							tell pop up button 1
								click
								click menu item 1 of menu 1
							end tell
							if (value of checkbox 1) = 1 then
								click checkbox 1
							end if
						else if (value of pop up button 1) = "I felt distracted" then
							tell pop up button 1
								click
								click menu item 2 of menu 1
							end tell
						end if
						click button logBtnName
					else if currentStep = "Prepare Timed Break" then
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