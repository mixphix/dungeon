import Std.Data.TreeMap.Basic

inductive Class where
  | barbarian
  | bard
  | cleric
  | druid
  | fighter
  | monk
  | paladin
  | ranger
  | rogue
  | sorcerer
  | warlock
  | wizard
  deriving BEq, DecidableEq, Ord, Repr

structure Level where level : Int
  deriving BEq, DecidableEq, Ord, Repr

def subclassLevel : Class -> Level :=
  fun cls => match cls with
  | Class.barbarian => {level := 3}
  | Class.bard => {level := 3}
  | Class.cleric => {level := 1}
  | Class.druid => {level := 3}
  | Class.fighter => {level := 3}
  | Class.monk => {level := 3}
  | Class.paladin => {level := 1}
  | Class.ranger => {level := 3}
  | Class.rogue => {level := 3}
  | Class.sorcerer => {level := 1}
  | Class.warlock => {level := 1}
  | Class.wizard => {level := 2}

inductive Subclass : Class -> Type where
  -- Barbarian
  | berserker : Subclass Class.barbarian
  | giant : Subclass Class.barbarian
  | wildMagicBarbarian : Subclass Class.barbarian
  | wildheart : Subclass Class.barbarian
  -- Bard
  | collegeOfGlamour : Subclass Class.bard
  | collegeOfLore : Subclass Class.bard
  | collegeOfSwords : Subclass Class.bard
  | collegeOfValour : Subclass Class.bard
  -- Cleric
  | deathDomain : Subclass Class.cleric
  | knowledgeDomain : Subclass Class.cleric
  | lifeDomain : Subclass Class.cleric
  | lightDomain : Subclass Class.cleric
  | natureDomain : Subclass Class.cleric
  | tempestDomain : Subclass Class.cleric
  | trickeryDomain : Subclass Class.cleric
  | warDomain : Subclass Class.cleric
  -- Druid
  | circleOfTheLand : Subclass Class.druid
  | circleOfTheMoon : Subclass Class.druid
  | circleOfTheSpores : Subclass Class.druid
  | circleOfTheStars : Subclass Class.druid
  -- Fighter
  | arcaneArcher : Subclass Class.fighter
  | battleMaster : Subclass Class.fighter
  | champion : Subclass Class.fighter
  | eldritchKnight : Subclass Class.fighter
  -- Monk
  | wayOfTheDrunkenMaster : Subclass Class.monk
  | wayOfTheFourElements : Subclass Class.monk
  | wayOfTheOpenHand : Subclass Class.monk
  | wayOfShadow : Subclass Class.monk
  -- Paladin
  | oathOfDevotion : Subclass Class.paladin
  | oathOfTheAncients : Subclass Class.paladin
  | oathOfTheCrown : Subclass Class.paladin
  | oathOfVengeance : Subclass Class.paladin
  | oathbreaker : Subclass Class.paladin
  -- Ranger
  | beastMaster : Subclass Class.ranger
  | gloomstalker : Subclass Class.ranger
  | hunter : Subclass Class.ranger
  | swarmkeeper : Subclass Class.ranger
  -- Rogue
  | arcaneTrickster : Subclass Class.rogue
  | assassin : Subclass Class.rogue
  | swashbuckler : Subclass Class.rogue
  | thief : Subclass Class.rogue
  -- Sorcerer
  | draconicBloodline : Subclass Class.sorcerer
  | shadowMagic : Subclass Class.sorcerer
  | stormSorcery : Subclass Class.sorcerer
  | wildMagicSorcerer : Subclass Class.sorcerer
  -- Warlock
  | theArchfey : Subclass Class.warlock
  | theFiend : Subclass Class.warlock
  | theGreatOldOne : Subclass Class.warlock
  | theHexblade : Subclass Class.warlock
  -- Wizard
  | schoolOfAbjuration : Subclass Class.wizard
  | schoolOfBladesinging : Subclass Class.wizard
  | schoolOfConjuration : Subclass Class.wizard
  | schoolOfDivination : Subclass Class.wizard
  | schoolOfEnchantment : Subclass Class.wizard
  | schoolOfEvocation : Subclass Class.wizard
  | schoolOfIllusion : Subclass Class.wizard
  | schoolOfNecromancy : Subclass Class.wizard
  | schoolOfTransmutation : Subclass Class.wizard
  deriving BEq, DecidableEq, Ord, Repr

inductive SpellSlot where
  | i
  | ii
  | iii
  | iv
  | v
  | vi
  | vii
  | viii
  | ix
  deriving BEq, DecidableEq, Ord, Repr

inductive Resource where
  | reaction
  | bonusAction
  | action
  | spellSlot : SpellSlot -> Resource
  | warlockSpellSlot : SpellSlot -> Resource
  | arcaneRecovery
  | bardicInspiration
  | bladesongPower
  | cosmicOmen
  | fungalInfestation
  | layOnHands
  | naturalRecovery
  | rage
  | sorceryPoint
  | starMap
  | warPriest
  | writhingCurrents
  | arcaneArrow
  | channelDivinity
  | channelOath
  | kiPoint
  | superiorityDie
  | tidesOfChaos
  | wildShape
  deriving BEq, DecidableEq, Ord, Repr

def leveledResource : Level -> Resource -> Int :=
  fun {level := lv} res => match res with
  | Resource.reaction => 1
  | Resource.bonusAction => 1
  | Resource.action => 1
  | Resource.spellSlot _ => 0
  | Resource.warlockSpellSlot _ => 0
  | Resource.arcaneRecovery => (lv + 1) / 2
  | Resource.bardicInspiration =>
    if lv < 5 then 3 else if lv < 8 then 4 else 5
  | Resource.bladesongPower =>
    if lv < 2 then 0 else if lv < 5 then 2 else if lv < 9 then 3 else 4
  | Resource.cosmicOmen =>
    if lv < 6 then 0 else if lv < 9 then 3 else 4
  | Resource.fungalInfestation =>
    if lv < 6 then 0 else 4
  | Resource.layOnHands =>
    if lv < 4 then 4 else if lv < 10 then 5 else 6
  | Resource.naturalRecovery => lv / 2
  | Resource.rage =>
    if lv < 3 then 2 else if lv < 6 then 6 else if lv < 12 then 4 else 5
  | Resource.sorceryPoint => if lv < 2 then 0 else lv
  | Resource.starMap =>
    if lv < 2 then 0 else if lv < 5 then 2 else if lv < 9 then 3 else 4
  | Resource.warPriest =>
    if lv < 5 then 3 else if lv < 8 then 4 else if lv < 11 then 5 else 6
  | Resource.writhingCurrents =>
    if lv < 7 then 0 else if lv < 9 then 3 else 4
  | Resource.arcaneArrow =>
    if lv < 3 then 0 else if lv < 7 then 4 else if lv < 10 then 7 else 10
  | Resource.channelDivinity =>
    if lv < 6 then 1 else 2
  | Resource.channelOath => 1
  | Resource.kiPoint => lv + 1
  | Resource.superiorityDie =>
    if lv < 7 then 4 else 5
  | Resource.tidesOfChaos => 1
  | Resource.wildShape => 2

inductive DOption (α : Type u) (β : α -> Type v) : Type (max u v) where
  | nothing
  | here : α -> DOption α β
  | there : (a : α) -> β a -> DOption α β

def classResources : Level -> DOption Class Subclass -> Std.TreeMap Resource Int :=
  fun lvl doption => match doption with
  | DOption.nothing =>
    {(Resource.reaction, 1), (Resource.bonusAction, 1), (Resource.action, 1)}
  | DOption.here Class.wizard =>
    {(Resource.arcaneRecovery, leveledResource lvl Resource.arcaneRecovery)}
  | DOption.here Class.bard =>
    {(Resource.bardicInspiration, leveledResource lvl Resource.bardicInspiration)}
  | DOption.there Class.wizard Subclass.schoolOfBladesinging =>
    {(Resource.bladesongPower, leveledResource lvl Resource.bladesongPower)}
  | DOption.there Class.druid Subclass.circleOfTheStars =>
    { (Resource.cosmicOmen, leveledResource lvl Resource.cosmicOmen)
    , (Resource.starMap, leveledResource lvl Resource.starMap)
    }
  | DOption.there Class.druid Subclass.circleOfTheSpores =>
    {(Resource.fungalInfestation, leveledResource lvl Resource.fungalInfestation)}
  | DOption.here Class.paladin =>
    { (Resource.layOnHands, leveledResource lvl Resource.layOnHands)
    , (Resource.channelOath, leveledResource lvl Resource.channelOath)
    }
  | DOption.there Class.druid Subclass.circleOfTheLand =>
    {(Resource.naturalRecovery, leveledResource lvl Resource.naturalRecovery)}
  | DOption.here Class.barbarian =>
    {(Resource.rage, leveledResource lvl Resource.rage)}
  | DOption.here Class.sorcerer =>
    {(Resource.sorceryPoint, leveledResource lvl Resource.sorceryPoint)}
  | DOption.there Class.cleric Subclass.warDomain =>
    {(Resource.warPriest, leveledResource lvl Resource.warPriest)}
  | DOption.there Class.ranger Subclass.swarmkeeper =>
    {(Resource.writhingCurrents, leveledResource lvl Resource.writhingCurrents)}
  | DOption.there Class.fighter Subclass.arcaneArcher =>
    {(Resource.arcaneArrow, leveledResource lvl Resource.arcaneArrow)}
  | DOption.here Class.cleric =>
    {(Resource.channelDivinity, leveledResource lvl Resource.channelDivinity)}
  | DOption.here Class.monk =>
    {(Resource.kiPoint, leveledResource lvl Resource.kiPoint)}
  | DOption.there Class.fighter Subclass.battleMaster =>
    {(Resource.superiorityDie, leveledResource lvl Resource.superiorityDie)}
  | DOption.there Class.sorcerer Subclass.wildMagicSorcerer =>
    {(Resource.tidesOfChaos, 1)}
  | DOption.here Class.druid =>
    {(Resource.wildShape, 2)}
  | _ => {}

inductive Feature : Level -> DOption Class Subclass -> Type where
  | rage : Feature {level := 1} (DOption.here Class.barbarian)
  | unarmouredDefenceBarbarian : Feature {level := 1} (DOption.here Class.barbarian)
  | recklessAttack : Feature {level := 2} (DOption.here Class.barbarian)
  | dangerSense : Feature {level := 2} (DOption.here Class.barbarian)
  | frenzy : Feature {level := 3} (DOption.there Class.barbarian Subclass.berserker)
  | frenziedStrike : Feature {level := 3} (DOption.there Class.barbarian Subclass.berserker)
  | enragedThrow : Feature {level := 3} (DOption.there Class.barbarian Subclass.berserker)

structure ClassInfo (cls : Class) where
  level : Level
  subclass : Option (Subclass cls)

structure Character where
  name : String
  classes : Std.DTreeMap Class ClassInfo
  resources : Std.TreeMap Resource Nat

def maxresources : Character -> Std.TreeMap Resource Int :=
  fun char =>
    char.classes.foldl
      ( fun acc cls ci => Std.TreeMap.union acc (match ci.subclass with
        | Option.none => classResources ci.level (DOption.here cls)
        | Option.some sub => classResources ci.level (DOption.there cls sub))
      )
      {(Resource.reaction, 1), (Resource.bonusAction, 1), (Resource.action, 1)}


-- class LevelSpellSlots (cls : Class) (sub : Subclass cls) where
--   levelSpellSlots : Nat -> Std.TreeMap SpellSlot Nat

-- instance : LevelSpellSlots Class.bard sub where
--   levelSpellSlots := fun n => match n with
--   | 1 => {(SpellSlot.i, 2)}
--   | 2 => {(SpellSlot.i, 3)}
--   | 3 => {(SpellSlot.i, 4), (SpellSlot.ii, 2)}
--   | 4 => {(SpellSlot.i, 4), (SpellSlot.ii, 3)}
--   | 5 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 2)}
--   | 6 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3)}
--   | 7 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 1)
--     }
--   | 8 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 2)
--     }
--   | 9 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 1)
--     }
--   | 10 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     }
--   | 11 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     }
--   | 12 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     }
--   | 13 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     }
--   | 14 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     }
--   | 15 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     }
--   | 16 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     }
--   | 17 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
--   | 18 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 3)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
--   | 19 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 3)
--     , (SpellSlot.vi, 2)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
--   | _ =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 3)
--     , (SpellSlot.vi, 2)
--     , (SpellSlot.vii, 2)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
-- instance : LevelSpellSlots Class.cleric sub where
--   levelSpellSlots := fun n => match n with
--   | 1 => {(SpellSlot.i, 2)}
--   | 2 => {(SpellSlot.i, 3)}
--   | 3 => {(SpellSlot.i, 4), (SpellSlot.ii, 2)}
--   | 4 => {(SpellSlot.i, 4), (SpellSlot.ii, 3)}
--   | 5 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 2)}
--   | 6 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3)}
--   | 7 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 1)
--     }
--   | 8 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 2)
--     }
--   | 9 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 1)
--     }
--   | 10 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     }
--   | 11 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     }
--   | 12 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     }
--   | 13 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     }
--   | 14 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     }
--   | 15 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     }
--   | 16 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     }
--   | 17 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
--   | 18 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 3)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
--   | 19 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 3)
--     , (SpellSlot.vi, 2)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
--   | _ =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 3)
--     , (SpellSlot.vi, 2)
--     , (SpellSlot.vii, 2)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
-- instance : LevelSpellSlots Class.druid sub where
--   levelSpellSlots := fun n => match n with
--   | 1 => {(SpellSlot.i, 2)}
--   | 2 => {(SpellSlot.i, 3)}
--   | 3 => {(SpellSlot.i, 4), (SpellSlot.ii, 2)}
--   | 4 => {(SpellSlot.i, 4), (SpellSlot.ii, 3)}
--   | 5 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 2)}
--   | 6 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3)}
--   | 7 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 1)
--     }
--   | 8 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 2)
--     }
--   | 9 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 1)
--     }
--   | 10 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     }
--   | 11 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     }
--   | 12 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     }
--   | 13 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     }
--   | 14 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     }
--   | 15 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     }
--   | 16 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     }
--   | 17 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
--   | 18 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 3)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
--   | 19 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 3)
--     , (SpellSlot.vi, 2)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
--   | _ =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 3)
--     , (SpellSlot.vi, 2)
--     , (SpellSlot.vii, 2)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
-- instance : LevelSpellSlots Class.fighter Subclass.eldritchKnight where
--   levelSpellSlots := fun n => match n with
--   | 3 => {(SpellSlot.i, 2)}
--   | 4 => {(SpellSlot.i, 3)}
--   | 5 => {(SpellSlot.i, 3)}
--   | 6 => {(SpellSlot.i, 3)}
--   | 7 => {(SpellSlot.i, 4), (SpellSlot.ii, 2)}
--   | 8 => {(SpellSlot.i, 4), (SpellSlot.ii, 2)}
--   | 9 => {(SpellSlot.i, 4), (SpellSlot.ii, 2)}
--   | 10 => {(SpellSlot.i, 4), (SpellSlot.ii, 3)}
--   | 11 => {(SpellSlot.i, 4), (SpellSlot.ii, 3)}
--   | 12 => {(SpellSlot.i, 4), (SpellSlot.ii, 3)}
--   | 13 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 2)}
--   | 14 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 2)}
--   | 15 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 2)}
--   | 16 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3)}
--   | 17 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3)}
--   | 18 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3)}
--   | _ => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3), (SpellSlot.iv, 1)}
-- instance : LevelSpellSlots Class.paladin sub where
--   levelSpellSlots := fun n => match n with
--   | 1 => {}
--   | 2 => {(SpellSlot.i, 2)}
--   | 3 => {(SpellSlot.i, 3)}
--   | 4 => {(SpellSlot.i, 3)}
--   | 5 => {(SpellSlot.i, 4), (SpellSlot.ii, 2)}
--   | 6 => {(SpellSlot.i, 4), (SpellSlot.ii, 2)}
--   | 7 => {(SpellSlot.i, 4), (SpellSlot.ii, 3)}
--   | 8 => {(SpellSlot.i, 4), (SpellSlot.ii, 3)}
--   | 9 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 2)}
--   | 10 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 2)}
--   | 11 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3)}
--   | 12 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3)}
--   | 13 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3), (SpellSlot.iv, 1)}
--   | 14 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3), (SpellSlot.iv, 1)}
--   | 15 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3), (SpellSlot.iv, 2)}
--   | 16 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3), (SpellSlot.iv, 2)}
--   | 17 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3), (SpellSlot.iv, 3), (SpellSlot.v, 1)}
--   | 18 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3), (SpellSlot.iv, 3), (SpellSlot.v, 1)}
--   | _ => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3), (SpellSlot.iv, 3), (SpellSlot.v, 2)}
-- instance : LevelSpellSlots Class.ranger sub where
--   levelSpellSlots := fun n => match n with
--   | 1 => {}
--   | 2 => {(SpellSlot.i, 2)}
--   | 3 => {(SpellSlot.i, 3)}
--   | 4 => {(SpellSlot.i, 3)}
--   | 5 => {(SpellSlot.i, 4), (SpellSlot.ii, 2)}
--   | 6 => {(SpellSlot.i, 4), (SpellSlot.ii, 2)}
--   | 7 => {(SpellSlot.i, 4), (SpellSlot.ii, 3)}
--   | 8 => {(SpellSlot.i, 4), (SpellSlot.ii, 3)}
--   | 9 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 2)}
--   | 10 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 2)}
--   | 11 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3)}
--   | 12 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3)}
--   | 13 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3), (SpellSlot.iv, 1)}
--   | 14 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3), (SpellSlot.iv, 1)}
--   | 15 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3), (SpellSlot.iv, 2)}
--   | 16 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3), (SpellSlot.iv, 2)}
--   | 17 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3), (SpellSlot.iv, 3), (SpellSlot.v, 1)}
--   | 18 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3), (SpellSlot.iv, 3), (SpellSlot.v, 1)}
--   | _ => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3), (SpellSlot.iv, 3), (SpellSlot.v, 2)}
-- instance : LevelSpellSlots Class.rogue Subclass.arcaneTrickster where
--   levelSpellSlots := fun n => match n with
--   | 3 => {(SpellSlot.i, 2)}
--   | 4 => {(SpellSlot.i, 3)}
--   | 5 => {(SpellSlot.i, 3)}
--   | 6 => {(SpellSlot.i, 3)}
--   | 7 => {(SpellSlot.i, 4), (SpellSlot.ii, 2)}
--   | 8 => {(SpellSlot.i, 4), (SpellSlot.ii, 2)}
--   | 9 => {(SpellSlot.i, 4), (SpellSlot.ii, 2)}
--   | 10 => {(SpellSlot.i, 4), (SpellSlot.ii, 3)}
--   | 11 => {(SpellSlot.i, 4), (SpellSlot.ii, 3)}
--   | 12 => {(SpellSlot.i, 4), (SpellSlot.ii, 3)}
--   | 13 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 2)}
--   | 14 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 2)}
--   | 15 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 2)}
--   | 16 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3)}
--   | 17 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3)}
--   | 18 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3)}
--   | _ => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3), (SpellSlot.iv, 1)}
-- instance : LevelSpellSlots Class.sorcerer sub where
--   levelSpellSlots := fun n => match n with
--   | 1 => {(SpellSlot.i, 2)}
--   | 2 => {(SpellSlot.i, 3)}
--   | 3 => {(SpellSlot.i, 4), (SpellSlot.ii, 2)}
--   | 4 => {(SpellSlot.i, 4), (SpellSlot.ii, 3)}
--   | 5 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 2)}
--   | 6 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3)}
--   | 7 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 1)
--     }
--   | 8 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 2)
--     }
--   | 9 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 1)
--     }
--   | 10 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     }
--   | 11 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     }
--   | 12 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     }
--   | 13 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     }
--   | 14 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     }
--   | 15 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     }
--   | 16 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     }
--   | 17 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
--   | 18 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 3)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
--   | 19 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 3)
--     , (SpellSlot.vi, 2)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
--   | _ =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 3)
--     , (SpellSlot.vi, 2)
--     , (SpellSlot.vii, 2)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
-- instance : LevelSpellSlots Class.warlock sub where
--   levelSpellSlots := fun n => match n with
--   | 1 => {(SpellSlot.i, 1)}
--   | 2 => {(SpellSlot.i, 2)}
--   | 3 => {(SpellSlot.ii, 2)}
--   | 4 => {(SpellSlot.ii, 2)}
--   | 5 => {(SpellSlot.iii, 2)}
--   | 6 => {(SpellSlot.iii, 2)}
--   | 7 => {(SpellSlot.iv, 2)}
--   | 8 => {(SpellSlot.iv, 2)}
--   | 9 => {(SpellSlot.v, 2)}
--   | 10 => {(SpellSlot.v, 2)}
--   | 11 => {(SpellSlot.v, 3)}
--   | 12 => {(SpellSlot.v, 3)}
--   | 13 => {(SpellSlot.v, 3)}
--   | 14 => {(SpellSlot.v, 3)}
--   | 15 => {(SpellSlot.v, 3)}
--   | _ => {(SpellSlot.v, 4)}
-- instance : LevelSpellSlots Class.wizard sub where
--   levelSpellSlots := fun n => match n with
--   | 1 => {(SpellSlot.i, 2)}
--   | 2 => {(SpellSlot.i, 3)}
--   | 3 => {(SpellSlot.i, 4), (SpellSlot.ii, 2)}
--   | 4 => {(SpellSlot.i, 4), (SpellSlot.ii, 3)}
--   | 5 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 2)}
--   | 6 => {(SpellSlot.i, 4), (SpellSlot.ii, 3), (SpellSlot.iii, 3)}
--   | 7 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 1)
--     }
--   | 8 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 2)
--     }
--   | 9 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 1)
--     }
--   | 10 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     }
--   | 11 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     }
--   | 12 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     }
--   | 13 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     }
--   | 14 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     }
--   | 15 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     }
--   | 16 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     }
--   | 17 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 2)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
--   | 18 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 3)
--     , (SpellSlot.vi, 1)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
--   | 19 =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 3)
--     , (SpellSlot.vi, 2)
--     , (SpellSlot.vii, 1)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
--   | _ =>
--     { (SpellSlot.i, 4)
--     , (SpellSlot.ii, 3)
--     , (SpellSlot.iii, 3)
--     , (SpellSlot.iv, 3)
--     , (SpellSlot.v, 3)
--     , (SpellSlot.vi, 2)
--     , (SpellSlot.vii, 2)
--     , (SpellSlot.viii, 1)
--     , (SpellSlot.ix, 1)
--     }
-- class Oathed (sub : Subclass Class.paladin)
-- instance : Oathed Subclass.oathOfDevotion where
-- instance : Oathed Subclass.oathOfTheAncients where
-- instance : Oathed Subclass.oathOfTheCrown where
-- instance : Oathed Subclass.oathOfVengeance where

--   -- | reaction : Resource cls sub
--   -- | bonusAction : Resource cls sub
--   -- | action : Resource cls sub
--   -- --
--   -- --
-- inductive Resource : (cls : Class) -> Subclass cls -> Type u where
--   | spellSlot : [LevelSpellSlots cls sub] -> SpellSlot -> Resource cls sub
--   | arcaneRecovery : Resource Class.wizard sub
--   | bardicInspiration : Resource Class.bard sub
--   | bladesongPower : Resource Class.wizard Subclass.schoolOfBladesinging
--   | cosmicOmen : Resource Class.druid Subclass.circleOfTheStars
--   | fungalInfestation : Resource Class.druid Subclass.circleOfTheSpores
--   | layOnHands : [Oathed sub] -> Resource Class.paladin sub
--   | naturalRecovery : Resource Class.druid Subclass.circleOfTheLand
--   | rage : Resource Class.barbarian sub
--   | sorceryPoint : Resource Class.sorcerer sub
--   | starMap : Resource Class.druid Subclass.circleOfTheStars
--   | warPriest : Resource Class.cleric Subclass.warDomain
--   | writhingCurrents : Resource Class.ranger Subclass.swarmkeeper
--   --
--   | arcaneArrow : Resource Class.fighter Subclass.arcaneArcher
--   | channelDivinity : Resource Class.cleric sub
--   | channelOath : Resource Class.paladin sub
--   | kiPoint : Resource Class.monk sub
--   | superiorityDie : Resource Class.fighter Subclass.battleMaster
--   | tidesOfChaos : Resource Class.sorcerer Subclass.wildMagicSorcerer
--   | wildShape : Resource Class.druid sub
-- instance cmp : Ord (Resource cls sub) where
--   compare := fun res0 res1 => match res0 with
--   | Resource.spellSlot s => match res1 with
--     | Resource.spellSlot s' => compare s s'
--     | _ => Ordering.lt
--   | Resource.arcaneRecovery => match res1 with
--     | Resource.arcaneRecovery => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.bardicInspiration => match res1 with
--     | Resource.bardicInspiration => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.bladesongPower => match res1 with
--     | Resource.arcaneRecovery => Ordering.gt
--     | Resource.bladesongPower => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.cosmicOmen => match res1 with
--     | Resource.cosmicOmen => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.fungalInfestation => match res1 with
--     | Resource.fungalInfestation => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.layOnHands => match res1 with
--     | Resource.layOnHands => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.naturalRecovery => match res1 with
--     | Resource.naturalRecovery => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.rage => match res1 with
--     | Resource.rage => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.sorceryPoint => match res1 with
--     | Resource.sorceryPoint => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.starMap => match res1 with
--     | Resource.cosmicOmen => Ordering.gt
--     | Resource.starMap => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.warPriest => match res1 with
--     | Resource.warPriest => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.writhingCurrents => match res1 with
--     | Resource.writhingCurrents => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.arcaneArrow => match res1 with
--     | Resource.arcaneArrow => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.channelDivinity => match res1 with
--     | Resource.warPriest => Ordering.gt
--     | Resource.channelDivinity => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.channelOath => match res1 with
--     | Resource.layOnHands => Ordering.gt
--     | Resource.channelOath => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.kiPoint => match res1 with
--     | Resource.kiPoint => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.superiorityDie => match res1 with
--     | Resource.superiorityDie => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.tidesOfChaos => match res1 with
--     | Resource.sorceryPoint => Ordering.gt
--     | Resource.tidesOfChaos => Ordering.eq
--     | _ => Ordering.lt
--   | Resource.wildShape => match res1 with
--     | Resource.cosmicOmen => Ordering.gt
--     | Resource.fungalInfestation => Ordering.gt
--     | Resource.naturalRecovery => Ordering.gt
--     | Resource.starMap => Ordering.gt
--     | Resource.wildShape => Ordering.eq
--     | _ => Ordering.lt

-- inductive Recharge where
--   | onTurn
--   | onShortRest
--   | onLongRest
--   deriving BEq, DecidableEq, Ord, Repr

-- def classResourceRecharge : Resource cls sub -> Recharge :=
--   fun res => match res with
--   | Resource.spellSlot _ => match cls with
--     | Class.warlock => Recharge.onShortRest
--     | _ => Recharge.onLongRest
--   | Resource.arcaneRecovery => Recharge.onLongRest
--   | Resource.bardicInspiration => Recharge.onLongRest
--   | Resource.bladesongPower => Recharge.onLongRest
--   | Resource.cosmicOmen => Recharge.onLongRest
--   | Resource.fungalInfestation => Recharge.onLongRest
--   | Resource.layOnHands => Recharge.onLongRest
--   | Resource.naturalRecovery => Recharge.onLongRest
--   | Resource.rage => Recharge.onLongRest
--   | Resource.sorceryPoint => Recharge.onLongRest
--   | Resource.starMap => Recharge.onLongRest
--   | Resource.warPriest => Recharge.onLongRest
--   | Resource.writhingCurrents => Recharge.onLongRest
--   --
--   | Resource.arcaneArrow => Recharge.onShortRest
--   | Resource.channelDivinity => Recharge.onShortRest
--   | Resource.channelOath => Recharge.onShortRest
--   | Resource.kiPoint => Recharge.onShortRest
--   | Resource.superiorityDie => Recharge.onShortRest
--   | Resource.tidesOfChaos => Recharge.onShortRest
--   | Resource.wildShape => Recharge.onShortRest

-- structure ClassInfo (cls : Class) where
--   level : Nat
--   subclass : Subclass cls

-- inductive Ability where
--   | strength
--   | dexterity
--   | constitution
--   | intelligence
--   | wisdom
--   | charisma
--   deriving BEq, DecidableEq, Ord, Repr

-- inductive Skill : Ability -> Type u where
--   | acrobatics : Skill Ability.dexterity
--   | animalHandling : Skill Ability.wisdom
--   | arcana : Skill Ability.intelligence
--   | athletics : Skill Ability.strength
--   | deception : Skill Ability.charisma
--   | history : Skill Ability.intelligence
--   | insight : Skill Ability.wisdom
--   | intimidation : Skill Ability.charisma
--   | investigation : Skill Ability.intelligence
--   | medicine : Skill Ability.wisdom
--   | nature : Skill Ability.intelligence
--   | perception : Skill Ability.wisdom
--   | performance : Skill Ability.charisma
--   | persuasion : Skill Ability.charisma
--   | religion : Skill Ability.intelligence
--   | sleightOfHand : Skill Ability.dexterity
--   | stealth : Skill Ability.dexterity
--   | survival : Skill Ability.wisdom
--   deriving BEq, DecidableEq, Ord, Repr

-- structure Character where
--   name : String
--   classInfo : Std.DTreeMap Class ClassInfo
--   baseAbilityScores : Std.TreeMap Ability Nat

-- def level : Character -> Nat :=
--   fun char => char.classInfo.foldl (fun acc _cls ci => acc + ci.level) 0
