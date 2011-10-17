module Build
( buildSequenceTable
, prop_last_number
, prop_preserves
)
where

import qualified Data.MultiSet as MultiSet
import qualified Data.List as List
import qualified Data.Ord as Ord
import Array

{-
    What I'm going to start with is probably going to be a set of reads, with a
    start and end index, and some list of bases.

    I need to build up some kind of data structure that lets me rapidly ask the
    question "what did we see in this parent at this index?"

    So, the way I did this in perl, IIRC, is an array holding multisets.  This
    could work fine, although an array holding named tuples would also be fine.
    I need this kind of arbitrary look up because the sequences I want to label
    are going to be at random locations.  A sparse array isn't useful because I'm
    expecting pretty high coverage.  Let's go with an array of multisets.

    Once I have my lookup done, I want to take each sequence and partition them
    into one for each parent by looking at how each position compares to the
    parent sequence and building up some sort of quality metric.

    Ok, so the first step is to generate the diffs from the reads.
-}

numberRead :: (Enum a) => (a, [b]) -> [(a, b)]
numberRead (start, read) = zip [start ..] read

prop_last_number (start, list) =
    let (last_digit, _) = last . numberRead $ (start, list) in
    last_digit == start + (length list) - 1

prop_preserves (start, list) =
    let (_, new_list) = unzip . numberRead $ (start, list) in
    list == new_list

{-  Once we have a list of diffs, accumulate it into a table of multisets of
    observed bases -}
buildSequenceTable :: (Enum a, Ix a, Ord b) =>
    a -> a -> [(a, [b])] -> Array a (MultiSet.MultiSet b)
buildSequenceTable start finish reads =
    accumArray f MultiSet.empty (start, finish) (concat.map numberRead $ reads)
    where f set addition = MultiSet.insert addition set

{-  Now that we can build these tables, we need to compare incoming reads against
    one of them.  We do this by building a quality metric by comparing a read
    against a table for one individual.-}
evaluate :: (Enum a, Ix a, Ord b) =>
    Array a (MultiSet.MultiSet b) -> (a, [b]) -> Int
evaluate table (start, read) =
    let numbered = numberRead (start, read) in
    let compare (index, symbol) = (+ (MultiSet.occur symbol (table ! index)))
    in
    foldr compare 0 numbered

{-  We then just pick the best one out of the mess -}
call :: (Enum a, Ix a, Ord b) =>
    [Array a (MultiSet.MultiSet b)] -> (a, [b]) ->
    (Int, Array a (MultiSet.MultiSet b))
call tables read =
    let values = map (\ table -> (evaluate table read, table)) tables in
    List.maximumBy (Ord.comparing fst) values
