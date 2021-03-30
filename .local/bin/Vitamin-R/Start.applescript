
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
				-- set startTimeSliceBtn to button "Start Time Slice"
				set startTimeSliceBtnName to "Start Time Slice"
				set logBtnName to "Log"
				set startTimedBreakBtnName to "Start Timed Break"
				set finishBtnName to "Finish" -- slice or break finish
				set hideBtnName to "Hide"
				-- set logAsCompletedCheckBoxName to  "Log as completed"
				
				set currentStep to ""
				set createTimeSliceOk to false
				
				repeat while createTimeSliceOk is not true
					set currentStep to getCurrentStep(mainWindow) of commonScript
					
					-- check whether start time slice button exist
					if currentStep = "Define Time Slice" then
						set objectiveArea to text area 1 of scroll area 1
						set tagsField to text field 1 -- tagsField set tagsField to text field 1 of text field 1 -- tagsField
						
						if argn > 0 then
							set value of objectiveArea to ""
							-- get the value of objectiveArea
							set value of objectiveArea to item 1 of argv
							-- get the value of objectiveArea
						end if
						
						if argn > 1 then
							set value of tagsField to ""
							get the value of tagsField
							set value of tagsField to item 2 of argv
							get the value of tagsField
							set focused of tagsField to true
							-- keystroke return
							set focused of objectiveArea to true
						else
							set value of tagsField to ""
							set focused of tagsField to true
							-- keystroke return
							set focused of objectiveArea to true
						end if
						
						set currentStep to getCurrentStep(mainWindow) of commonScript
						if currentStep = "Define Time Slice" then
							click button startTimeSliceBtnName
						end if
						
						-- click button hideBtnName
						set createTimeSliceOk to true
						hideWindow(mainWindow) of commonScript
						return "OK"
					else if currentStep = "Rate Time Slice" then
						click button logBtnName
					else if currentStep = "Prepare Timed Break" then
						click button startTimedBreakBtnName
					else if currentStep = "Time Slice Running..." or currentStep = "On a Timed Break..." or currentStep = "Time Slice Paused." then
						-- log currentStep
						-- click button finishBtnName
						hideWindow(mainWindow) of commonScript
						log "Time Slice or Break is Running ... "
						return "OK"
					else
						display dialog "Cannot start new time slice:" & currentStep
					end if
				end repeat
			end tell
		end tell
	end tell
end run