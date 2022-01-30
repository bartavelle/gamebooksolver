module LoneWolf.Simplify (extractMultiFight) where

import Control.Lens
import Data.Either (partitionEithers)
import LoneWolf.Chapter
import LoneWolf.Character (Flag (HadCombat))

data ChapterSimplInfo = CI ChapterId String String (ChapterOutcome -> Decision) [FightDetails] ChapterOutcome

separateOutcome :: Decision -> Maybe (ChapterOutcome -> Decision, ChapterOutcome)
separateOutcome d =
  case d of
    Decisions _ -> Nothing
    CanTake i c sub -> appendDec (CanTake i c) sub
    Canbuy i p sub -> appendDec (Canbuy i p) sub
    Cansell i p sub -> appendDec (Cansell i p) sub
    Conditional bc sub -> appendDec (Conditional bc) sub
    Special _ -> Nothing
    NoDecision co -> Just (NoDecision, co)
    EvadeFight r cid fd oc -> Just (EvadeFight r cid fd, oc)
    AfterCombat sub -> appendDec AfterCombat sub
    RetrieveEquipment sub -> appendDec RetrieveEquipment sub
    RemoveItemFrom s c sub -> appendDec (RemoveItemFrom s c) sub
    LoseItemFrom s c sub -> appendDec (LoseItemFrom s c) sub
  where
    appendDec :: (Decision -> Decision) -> Decision -> Maybe (ChapterOutcome -> Decision, ChapterOutcome)
    appendDec f sub = do
      (chain, co) <- separateOutcome sub
      pure (f . chain, co)

extractSubfights :: ChapterOutcome -> ([FightDetails], ChapterOutcome)
extractSubfights oc =
  case oc of
    Fight fd nxt ->
      let (nf, fin) = extractSubfights nxt
       in (fd : nf, fin)
    _ -> ([], oc)

extractMultiFight :: [(ChapterId, Chapter)] -> [(ChapterId, Chapter)]
extractMultiFight chaps = std ++ mkMulti (maxid + 1) multi
  where
    (multi, std) = partitionEithers (map isMulti chaps)
    isMulti :: (ChapterId, Chapter) -> Either ChapterSimplInfo (ChapterId, Chapter)
    isMulti ch@(cid, Chapter nm dsc c) =
      case separateOutcome c of
        Nothing -> Right ch
        Just (mkdec, co) ->
          case extractSubfights co of
            ([], _) -> Right ch
            (fights, fin) -> Left (CI cid nm dsc mkdec (setFollowingFights fights) fin)
    maxid = maximum (map fst chaps)
    setFollowingFights [] = []
    setFollowingFights [x] = [x]
    setFollowingFights (f : fs) = (f & fightMod %~ (MultiFight :)) : setFollowingFights fs

    mkMulti :: ChapterId -> [ChapterSimplInfo] -> [(ChapterId, Chapter)]
    mkMulti _ [] = []
    mkMulti cid (c : cs) =
      let (cid', o) = mkSimplified cid c
       in o ++ mkMulti cid' cs

    mkSimplified :: ChapterId -> ChapterSimplInfo -> (ChapterId, [(ChapterId, Chapter)])
    mkSimplified cid (CI rcid n d mkdec details oc) =
      ( lastsol + 2,
        (rcid, Chapter n d (mkdec (hadcombat (Goto cid)))) : sols ++ [dummyChapter (lastsol + 1) (hadcombat oc)]
      )
      where
        hadcombat = Simple [SetFlag HadCombat]
        dummyChapter chid oc' = (chid, Chapter (show chid) ("Dummy chapter " ++ show chid) (NoDecision oc'))
        lastsol = fst (last sols)
        sols = zipWith (\chid fd -> dummyChapter chid (Fight fd (Goto (chid + 1)))) [cid ..] details