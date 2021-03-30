-- get current state of Vitamin-R 
-- sequence below：
-- Define Time Slice/Start Time Slice
-- Time Slice Running.../
-- Time Slice Paused.
-- Rate Time Slice
-- Prepare Timed Break/Start Timed Break
-- On a Timed Break...
on getCurrentStep(mainWindow)
	set currentStep to ""
	tell application "System Events"
		tell mainWindow
			set startTimeSliceBtnName to "Start Time Slice"
			set logBtnName to "Log"
			set startTimedBreakBtnName to "Start Timed Break"
			set finishBtnName to "Finish" -- slice or break finish
			set resumeBtnName to "Resume"
			-- Rate Time Slice
			
			if (first button whose name is startTimeSliceBtnName) of mainWindow exists then
				set currentStep to "Define Time Slice"
			else if (first button whose name is logBtnName) of mainWindow exists then
				set currentStep to "Rate Time Slice"
			else if (first button whose name is startTimedBreakBtnName) of mainWindow exists then
				set currentStep to "Prepare Timed Break"
			else if (first button whose name is resumeBtnName) of mainWindow exists then
				set currentStep to "Time Slice Paused."
			else if (first button whose name is finishBtnName) of mainWindow exists then
				set allSliceStaticTexts to value of every static text of mainWindow
				set elapsedSec to 0
				set remainSec to 0
				repeat with idx from 1 to count of allSliceStaticTexts
					log idx & item idx of allSliceStaticTexts
					if idx = 2 then
						set elapsedSec to convertStaticTextToSeconds(item idx of allSliceStaticTexts) of me
					else if idx = 6 then
						set remainSec to convertStaticTextToSeconds(item idx of allSliceStaticTexts) of me
					else if idx = 8 then
						if item idx of allSliceStaticTexts is "25′00″" or (elapsedSec + remainSec)  = 1499 then
							set currentStep to "Time Slice Running..."
						else if item idx of allSliceStaticTexts is "05′00″" then
							set currentStep to "On a Timed Break..."
						end if
						exit repeat
					end if
				end repeat
			end if
		end tell
	end tell
	return currentStep
end getCurrentStep

on hideWindow(mainWindow)
	tell application "System Events"
		tell mainWindow
			set hideBtnName to "Hide"
			if (first button whose name is hideBtnName) of mainWindow exists then
				click button hideBtnName
			end if
		end tell
	end tell
end hideWindow


on convertStaticTextToSeconds(t)
	-- set t to "25′00″"
	set sLen to (get length of t)
	if sLen ≠ 6 then
		return 0
	end if
	
	set min to (text 2 thru (1) of t as number)
	set sec to (text 4 thru (5) of t as number)
	return min * 60 + sec
end convertStaticTextToSeconds
