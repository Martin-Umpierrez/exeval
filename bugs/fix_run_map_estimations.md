
# Bug Fix: `run_MAP_estimations` Function

## Issue
The function `run_MAP_estimations` was not correctly generating all event times needed for posterior simulations. 
Previously, it only included the time points corresponding to treatment administration. As a result, other expected time points were missing during simulations.

## Fix
The issue has been resolved. Now, the function correctly includes all relevant time points, ensuring accurate posterior simulations.

## Affected Version
- Fixed in version **0.0.0.9000**

## Related Files
- `R/run_map_estimations.R`  
- Other files if applicable...

