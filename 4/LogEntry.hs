module LogEntry where

import Data.Time.Clock
import Data.Time.Format
import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry { logLevel :: LogLevel, timestamp :: UTCTime, message :: String }

logLevelToString :: LogLevel -> String
logLevelToString Error = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info = "Info"

logEntryToString :: LogEntry -> String
logEntryToString e = (timeToString $ timestamp $ e) ++ ": " 
    ++ (logLevelToString $ logLevel $ e) ++ ": " 
    ++ (message $ e)