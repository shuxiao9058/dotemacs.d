-- 获取 Vitamin 当前执行阶段
-- 按照时间顺序主要有如下：
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
			
			-- set createTimeSliceOk to false
			-- display dialog currentStep
			
			-- repeat while createTimeSliceOk ≠ true
			-- check whether start time slice button exist
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
				repeat with idx from 1 to count of allSliceStaticTexts
					-- if item i of myValues is myTarget then exit repeat
					if idx = 8 then
						if item idx of allSliceStaticTexts is "25′00″" then
							set currentStep to "Time Slice Running..."
							-- vitamin is running
							-- return -1
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
