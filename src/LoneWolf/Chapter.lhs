> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE DeriveDataTypeable #-}
> module LoneWolf.Chapter where

> import LoneWolf.Character
> import Control.Lens
> import Data.Data
> import Data.Data.Lens

> type Proba = Rational
> type ChapterId = Int
> type Rounds = Int
> type Price = Int

> data ChapterOutcome = Randomly [(Proba, ChapterOutcome)]
>                     | Conditionally [(BoolCond, ChapterOutcome)]
>                     | Fight FightDetails ChapterOutcome
>                     | EvadeFight Rounds ChapterId FightDetails ChapterOutcome
>                     | DamagePlayer Int ChapterOutcome
>                     | HealPlayer Int ChapterOutcome
>                     | FullHeal ChapterOutcome
>                     | HalfHeal ChapterOutcome
>                     | GainItem Item Int ChapterOutcome
>                     | LoseItem Item Int ChapterOutcome
>                     | LoseItemKind [Slot] ChapterOutcome
>                     | MustEat CanHunt ChapterOutcome
>                     | Goto ChapterId
>                     | GameLost
>                     | GameWon
>                     deriving (Show, Eq, Typeable, Data)

> data CanHunt = Hunt | NoHunt
>              deriving (Show, Eq, Typeable, Data)

> data Decision = Decisions [(String, Decision)]
>               | NoDecision ChapterOutcome
>               | CanTake Item Int Decision
>               | Conditional BoolCond Decision
>               | Canbuy Item Price Decision
>               | Cansell Item Price Decision
>               | Special SpecialChapter
>               deriving (Show, Eq, Typeable, Data)

> data SpecialChapter = Cartwheel
>                     | Portholes
>                     deriving (Show, Eq, Typeable, Data)

> data Chapter = Chapter { _title   :: String
>                        , _desc    :: String
>                        , _pchoice :: Decision
>                        } deriving (Show, Eq)

> data FightDetails = FightDetails
>           { _opponent     :: String
>           , _fcombatSkill :: CombatSkill
>           , _fendurance   :: Endurance
>           , _fightMod     :: [FightModifier]
>           } deriving (Show, Eq, Typeable, Data)

> data FightModifier = Undead
>                    | MindblastImmune
>                    | Timed Int FightModifier
>                    | CombatBonus CombatSkill
>                    | BareHanded
>                    | FakeFight ChapterId
>                    | EnemyMindblast
>                    | PlayerInvulnerable
>                    | DoubleDamage -- chapter 306
>                    deriving (Show, Eq, Typeable, Data)

> data BoolCond = HasDiscipline Discipline
>               | Not BoolCond
>               | HasEndurance Int
>               | COr BoolCond BoolCond
>               | CAnd BoolCond BoolCond
>               | HasItem Item Int
>               | Always Bool
>               deriving (Show, Eq, Typeable, Data)

> botherwise :: BoolCond
> botherwise = Always True

> (.&&.) :: BoolCond -> BoolCond -> BoolCond
> (.&&.) = CAnd

> makePrisms ''ChapterOutcome
> makePrisms ''Decision
> makeLenses ''FightDetails

> moneyCond :: Int -> ChapterOutcome -> Decision
> moneyCond price = Conditional (HasItem Gold price) . NoDecision . LoseItem Gold price

> outcomePlate :: Traversal' Decision ChapterOutcome
> outcomePlate = biplate
