# tba-haskell
This library is a WIP toolset for using The Blue Alliance public API in Haskell. 
Working demos can be found in `src/Main.hs`. 

## Sample Usage
The following IO operation calculates a team's scoring average at an event and uses most of the features in this library:
```haskell
import Lib (mean)

import TBA
import TBA.Match

import Control.Lens
import Network.Wreq (responseBody)

showTeamAverageScore :: IO()
showTeamAverageScore = do
    -- read config from stdin
    client <- createClient
    eventName <- ask "Enter event code: (e.g. 2019scmb)"
    teamKey <- askT "Enter team key: (e.g. frc254)"

    -- make api request
    matches <- tbaEventMatches eventName client

    -- access nested data and convert it to needed format
    let scores = matches ^.. responseBody . traverse . teamScore teamKey . totalPoints
    let scoresF = fromIntegral <$> scores

    -- print results
    putStrLn $ "Scores: " ++ (show scores)
    putStrLn $ "Total: " ++  (show $ sum scoresF)
    putStrLn $ "Average: " ++ (show $ mean scoresF)
```
Walking through this code, we note:
* `createClient` automates the process of asking the user for a TBA Auth Code, reading in from `stdin`, and generating a `TBAClient` instance using `tbaNew`.
* We get the event code and team key by using the `ask` and `askT` helper functions (see `src/Main.hs`).
* We make the request using `tbaEventMatches`, which takes the event name and our client as parameters. This returns `IO (Response [Match])`, so we "pull out" the `Response` using the monadic arrow notation.
* We then need to access the data inside the `matches` instance. To pull out this deeply nested info, we use the traverse operator (`^..`) from the `lens` library and a pipeline of lenses, giving us an interface similar to OOP. The lenses work thusly:
  - `responseBody` - gives us access to the parsed `[Match]` list inside our response object.
  - `traverse` - In combination with the traverse operator, this applies the following lenses to all of the matches in the list and concatenates the results into a list.
  - `teamScore` - accesses the `MatchScoreBreakdown` for the team key argument passed. If the team is not invovled is this match, it gets ignored in the traversal.
  - `totalPoints` - A plain 'getter' lens that accesses the `totalPoints` field in the breakdown object.
* We convert the list of `Int`s we got from the last step into a list of `Float`s by mapping `fromIntegral` over the list.
* We can then compute statistics for this list and print them to the console.

## Supported Models
* `Match`
    - `MatchScoreBreakdown`
        * 2018 and 2019 directly supported

## Supported Requests
* `tbaDistrictMatches :: Int -> District -> TBARequest [Match]`
* `tbaEventMatches :: String -> TBARequest [Match]`
* `tbaSingleMatch :: String -> TBARequest Match`