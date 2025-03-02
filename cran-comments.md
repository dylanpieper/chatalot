## R CMD check results
0 errors | 0 warnings | 1 note

* This is a new release.

## Notes
* There is one NOTE: "File 'hellmer/R/attach.R': .onAttach calls: library("ellmer")".
  This is intentional as this package is designed to attach 'ellmer' when loaded, 
  similar to how the 'tidyverse' package works. For this package to work, users need
  'ellmer' to pass a chat model (e.g., chat_claude) to the functions 
  'chat_sequential' and 'chat_future'.