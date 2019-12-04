module History where
import Room

type History = [Room]

emptyHistory :: History
emptyHistory = []

record :: Room -> History -> History
record = (:)

at :: History -> Int -> Room
h `at` n = h !! ((negate n) - 1)
