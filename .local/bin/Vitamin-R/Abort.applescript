
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
				set finishBtnName to "Finish"
				set hideBtnName to "Hide"
				set pauseBtnName to "Pause"
				set abortBtnName to "Abort"
				
				set currentStep to ""
				set isStoped to false
				set isAborted to false
				
				repeat while isAborted is not true
					set currentStep to getCurrentStep(mainWindow) of commonScript
					log currentStep
					
					if currentStep = "Time Slice Paused." then
						click button abortBtnName
						delay 0.2
						tell process appName to  keystroke return
						set isAborted to true
					else if currentStep = "Time Slice Running..." or currentStep = "On a Timed Break..." then
						click button pauseBtnName
					else if currentStep = "Define Time Slice" then
						set isStoped to true
					else
						return "ILLEGAL STATE"
					end if
					
					if isAborted or isStoped then
						hideWindow(mainWindow) of commonScript
						return "OK"
					end if
				end repeat
			end tell
		end tell
	end tell
end run