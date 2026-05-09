
# Mayor Change in function : `run_MAP_estimations` Function

## Issue
The function `run_MAP_estimations` was not performing well when data missing Steady State.

## Fix
The issue has been resolved. Now, the function correctly accumulate past events when Steady State is missing.

## Affected Version
- Fixed in version **0.0.0.9000**.

## Related Files
- `R/run_map_estimations.R`  
- Other files if applicable...

