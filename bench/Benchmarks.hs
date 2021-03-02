import           Criterion.Main
import qualified Problems.P01Bench as P01
import qualified Problems.P02Bench as P02
import qualified Problems.P03Bench as P03
import qualified Problems.P04Bench as P04
import qualified Problems.P05Bench as P05
import qualified Problems.P06Bench as P06
import qualified Problems.P07Bench as P07
import qualified Problems.P08Bench as P08
import qualified Problems.P09Bench as P09
import qualified Problems.P10Bench as P10
import qualified Problems.P11Bench as P11
import qualified Problems.P12Bench as P12
import qualified Problems.P13Bench as P13
import qualified Problems.P14Bench as P14
import qualified Problems.P15Bench as P15
import qualified Problems.P16Bench as P16
import qualified Problems.P17Bench as P17
import qualified Problems.P31Bench as P31
import qualified Problems.P32Bench as P32
import qualified Problems.P33Bench as P33
import qualified Problems.P34Bench as P34
import qualified Problems.P35Bench as P35
import qualified Problems.P36Bench as P36
import qualified Problems.P37Bench as P37
import qualified Problems.P39Bench as P39
import qualified Problems.P46Bench as P46
import qualified Problems.P48Bench as P48
import qualified Problems.P49Bench as P49
import qualified Problems.P55Bench as P55
import qualified Problems.P56Bench as P56
import qualified Problems.P57Bench as P57
import qualified Problems.P80Bench as P80
import qualified Problems.P81Bench as P81
import qualified Problems.P82Bench as P82
import qualified Problems.P90Bench as P90

main :: IO()
main = defaultMain [ P01.group
                   , P02.group
                   , P03.group
                   , P04.group
                   , P05.group
                   , P06.group
                   , P07.group
                   , P08.group
                   , P09.group
                   , P10.group
                   , P11.group
                   , P12.group
                   , P13.group
                   , P14.group
                   , P15.group
                   , P16.group
                   , P17.group
                   , P31.group
                   , P32.group
                   , P33.group
                   , P34.group
                   , P35.group
                   , P36.group
                   , P37.group
                   , P39.group
                   , P46.group
                   , P48.group
                   , P49.group
                   -- P54 benchmark intentionally omitted
                   , P55.group
                   , P56.group
                   , P57.group
                   , P80.group
                   , P81.group
                   , P82.group
                   , P90.group
                   ]
