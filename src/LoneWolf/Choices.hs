module LoneWolf.Choices where

import Control.Lens

import LoneWolf.Character
import LoneWolf.Chapter
import LoneWolf.Rules (check)

importantItem :: Item -> CharacterConstant -> CharacterVariable -> Bool
importantItem i cconstant cvariable =
    case i of
        Weapon Sommerswerd -> True
        Weapon MagicSpear -> True
        Weapon w | hasGoodweapon -> False

overflowItem :: Item -> CharacterVariable -> Bool
overflowItem = undefined

flattenDecision :: CharacterConstant -> CharacterVariable -> Decision -> [([String], ChapterOutcome)]
flattenDecision cconstant cvariable d =
  let inventory = cvariable ^. equipment
  in  case d of
        Conditional bc d' -> if check cconstant cvariable bc
                                then flattenDecision cconstant cvariable d'
                                else []
        Special _ -> error "Special chapters not handled yet"
        NoDecision o -> [([], o)]
        EvadeFight nrounds cid fdetails co -> [ (["no evasion"], Fight fdetails co)
                                              , (["evasion"], Fight (fdetails & fightMod %~ (Evaded nrounds cid :)) co)
                                              ]
        Decisions lst -> do
            (desc, d') <- lst
            (alldesc, o) <- flattenDecision cconstant cvariable d'
            return (desc : alldesc, o)
        CanTake item count nxt
            | item == Gold -> flattenDecision cconstant (cvariable & equipment . gold +~ count) nxt
        Canbuy item price nxt
            | cvariable ^. equipment . gold < price -> flattenDecision cconstant cvariable nxt
        Cansell item price nxt
            | not (hasItem item inventory) -> flattenDecision cconstant cvariable nxt
