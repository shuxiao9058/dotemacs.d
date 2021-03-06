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
			set pauseBtnName to "Pause"
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
					set staticText to item idx of allSliceStaticTexts
					log idx & staticText
					if idx = 2 then
						set elapsedSec to convertStaticTextToSeconds(staticText) of me
					else if idx = 6 then
						set remainSec to convertStaticTextToSeconds(staticText) of me
					else if idx = 8 then
						-- set pauseBtn to (first button whose name is pauseBtnName) of mainWindow
						set isExistPauseBtn to isExistBtn(mainWindow, pauseBtnName) of me
						if staticText is "25′00″" or (elapsedSec + remainSec) = 1499 and isExistPauseBtn then
							set currentStep to "Time Slice Running..."
						else if convertStaticTextToSeconds(staticText) of me > 0 and not isExistPauseBtn then
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

on isExistBtn(mainWindow, btnName)
	tell application "System Events"
		tell mainWindow
			set allBtns to name of every button of mainWindow
			log allBtns
			repeat with idx from 1 to count of allBtns
				set itemBtnName to item idx of allBtns
				if itemBtnName = btnName then
					return true
				end if
			end repeat
		end tell
	end tell
	
	return false
end isExistBtn

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

on breakBtnNameWithPomoCount(pomoCount)
	if pomoCount = "1" then
		return "1st Break"
	else if pomoCount = "2" then
		return "2nd Break"
	else if pomoCount = "3" then
		return "3rd Break"
	else if pomoCount = "0" then
		return "4th Break"
	end if
	
	return "1st Break"
end breakBtnNameWithPomoCount

to splitText(myText, delimiter)
	set oldDelimiters to AppleScript's text item delimiters
	set AppleScript's text item delimiters to delimiter
	-- set someText to someText's text items
	set resultItems to every text item of myText
	set AppleScript's text item delimiters to oldDelimiters --> restore delimiters to default value
	return resultItems
end splitText
