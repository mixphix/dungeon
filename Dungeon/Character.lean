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

inductive DSpecify (x : Type u) (y : x -> Type v) : Type (max u v) where
  | sans : x -> DSpecify x y
  | avec : (a : x) -> y a -> DSpecify x y

def fullCaster : Level -> Std.TreeMap Resource Int :=
  fun {level := lvl} => match lvl with
  | 1 => {(Resource.spellSlot SpellSlot.i, 2)}
  | 2 => {(Resource.spellSlot SpellSlot.i, 3)}
  | 3 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 2)
    }
  | 4 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    }
  | 5 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 2)
    }
  | 6 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    }
  | 7 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 1)
    }
  | 8 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 2)
    }
  | 9 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 3)
    , (Resource.spellSlot SpellSlot.v, 1)
    }
  | 10 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 3)
    , (Resource.spellSlot SpellSlot.v, 2)
    }
  | 11 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 3)
    , (Resource.spellSlot SpellSlot.v, 2)
    , (Resource.spellSlot SpellSlot.vi, 1)
    }
  | 12 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 3)
    , (Resource.spellSlot SpellSlot.v, 2)
    , (Resource.spellSlot SpellSlot.vi, 1)
    }
  | 13 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 3)
    , (Resource.spellSlot SpellSlot.v, 2)
    , (Resource.spellSlot SpellSlot.vi, 1)
    , (Resource.spellSlot SpellSlot.vii, 1)
    }
  | 14 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 3)
    , (Resource.spellSlot SpellSlot.v, 2)
    , (Resource.spellSlot SpellSlot.vi, 1)
    , (Resource.spellSlot SpellSlot.vii, 1)
    }
  | 15 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 3)
    , (Resource.spellSlot SpellSlot.v, 2)
    , (Resource.spellSlot SpellSlot.vi, 1)
    , (Resource.spellSlot SpellSlot.vii, 1)
    , (Resource.spellSlot SpellSlot.viii, 1)
    }
  | 16 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 3)
    , (Resource.spellSlot SpellSlot.v, 2)
    , (Resource.spellSlot SpellSlot.vi, 1)
    , (Resource.spellSlot SpellSlot.vii, 1)
    , (Resource.spellSlot SpellSlot.viii, 1)
    }
  | 17 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 3)
    , (Resource.spellSlot SpellSlot.v, 2)
    , (Resource.spellSlot SpellSlot.vi, 1)
    , (Resource.spellSlot SpellSlot.vii, 1)
    , (Resource.spellSlot SpellSlot.viii, 1)
    , (Resource.spellSlot SpellSlot.ix, 1)
    }
  | 18 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 3)
    , (Resource.spellSlot SpellSlot.v, 3)
    , (Resource.spellSlot SpellSlot.vi, 1)
    , (Resource.spellSlot SpellSlot.vii, 1)
    , (Resource.spellSlot SpellSlot.viii, 1)
    , (Resource.spellSlot SpellSlot.ix, 1)
    }
  | 19 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 3)
    , (Resource.spellSlot SpellSlot.v, 3)
    , (Resource.spellSlot SpellSlot.vi, 2)
    , (Resource.spellSlot SpellSlot.vii, 1)
    , (Resource.spellSlot SpellSlot.viii, 1)
    , (Resource.spellSlot SpellSlot.ix, 1)
    }
  | 20 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 3)
    , (Resource.spellSlot SpellSlot.v, 3)
    , (Resource.spellSlot SpellSlot.vi, 2)
    , (Resource.spellSlot SpellSlot.vii, 2)
    , (Resource.spellSlot SpellSlot.viii, 1)
    , (Resource.spellSlot SpellSlot.ix, 1)
    }
  | _ => {}

def halfCaster : Level -> Std.TreeMap Resource Int :=
  fun {level := lvl} => match lvl with
  | 1 => {}
  | 2 => {(Resource.spellSlot SpellSlot.i, 2)}
  | 3 => {(Resource.spellSlot SpellSlot.i, 3)}
  | 4 => {(Resource.spellSlot SpellSlot.i, 3)}
  | 5 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 2)
    }
  | 6 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 2)
    }
  | 7 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    }
  | 8 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    }
  | 9 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 2)
    }
  | 10 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 2)
    }
  | 11 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    }
  | 12 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    }
  | 13 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 1)
    }
  | 14 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 1)
    }
  | 15 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 2)
    }
  | 16 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 2)
    }
  | 17 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 3)
    , (Resource.spellSlot SpellSlot.v, 1)
    }
  | 18 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 3)
    , (Resource.spellSlot SpellSlot.v, 1)
    }
  | 19 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 3)
    , (Resource.spellSlot SpellSlot.v, 2)
    }
  | 20 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 3)
    , (Resource.spellSlot SpellSlot.v, 2)
    }
  | _ => {}

def thirdCaster : Level -> Std.TreeMap Resource Int :=
  fun {level := lvl} => match lvl with
  | 1 => {}
  | 2 => {}
  | 3 => {(Resource.spellSlot SpellSlot.i, 2)}
  | 4 => {(Resource.spellSlot SpellSlot.i, 3)}
  | 5 => {(Resource.spellSlot SpellSlot.i, 3)}
  | 6 => {(Resource.spellSlot SpellSlot.i, 3)}
  | 7 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 2)
    }
  | 8 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 2)
    }
  | 9 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 2)
    }
  | 10 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    }
  | 11 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    }
  | 12 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    }
  | 13 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 2)
    }
  | 14 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 2)
    }
  | 15 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 2)
    }
  | 16 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    }
  | 17 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    }
  | 18 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    }
  | 19 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 1)
    }
  | 20 =>
    { (Resource.spellSlot SpellSlot.i, 4)
    , (Resource.spellSlot SpellSlot.ii, 3)
    , (Resource.spellSlot SpellSlot.iii, 3)
    , (Resource.spellSlot SpellSlot.iv, 1)
    }
  | _ => {}

def warlockSpells : Level -> Std.TreeMap Resource Int :=
  fun {level := lvl} => match lvl with
  | 1 => {(Resource.warlockSpellSlot SpellSlot.i, 1)}
  | 2 => {(Resource.warlockSpellSlot SpellSlot.i, 2)}
  | 3 => {(Resource.warlockSpellSlot SpellSlot.ii, 2)}
  | 4 => {(Resource.warlockSpellSlot SpellSlot.ii, 2)}
  | 5 => {(Resource.warlockSpellSlot SpellSlot.iii, 2)}
  | 6 => {(Resource.warlockSpellSlot SpellSlot.iii, 2)}
  | 7 => {(Resource.warlockSpellSlot SpellSlot.iv, 2)}
  | 8 => {(Resource.warlockSpellSlot SpellSlot.iv, 2)}
  | 9 => {(Resource.warlockSpellSlot SpellSlot.v, 2)}
  | 10 => {(Resource.warlockSpellSlot SpellSlot.v, 2)}
  | 11 => {(Resource.warlockSpellSlot SpellSlot.v, 3)}
  | 12 => {(Resource.warlockSpellSlot SpellSlot.v, 3)}
  | 13 => {(Resource.warlockSpellSlot SpellSlot.v, 3)}
  | 14 => {(Resource.warlockSpellSlot SpellSlot.v, 3)}
  | 15 => {(Resource.warlockSpellSlot SpellSlot.v, 3)}
  | 16 => {(Resource.warlockSpellSlot SpellSlot.v, 3)}
  | 17 => {(Resource.warlockSpellSlot SpellSlot.v, 4)}
  | 18 => {(Resource.warlockSpellSlot SpellSlot.v, 4)}
  | 19 => {(Resource.warlockSpellSlot SpellSlot.v, 4)}
  | 20 => {(Resource.warlockSpellSlot SpellSlot.v, 4)}
  | _ => {}

def leveledSpellSlots : Level -> DSpecify Class Subclass -> Std.TreeMap Resource Int :=
  fun lvl dspecify => match dspecify with
  | DSpecify.sans Class.bard => fullCaster lvl
  | DSpecify.sans Class.cleric => fullCaster lvl
  | DSpecify.sans Class.druid => fullCaster lvl
  | DSpecify.sans Class.paladin => halfCaster lvl
  | DSpecify.sans Class.ranger => halfCaster lvl
  | DSpecify.sans Class.sorcerer => fullCaster lvl
  | DSpecify.sans Class.warlock => warlockSpells lvl
  | DSpecify.sans Class.wizard => fullCaster lvl
  | DSpecify.avec Class.rogue Subclass.arcaneTrickster => thirdCaster lvl
  | DSpecify.avec Class.fighter Subclass.eldritchKnight => thirdCaster lvl
  | _ => {}

def classResources : Level -> DSpecify Class Subclass -> Std.TreeMap Resource Int :=
  fun lvl dspecify => match dspecify with
  | DSpecify.sans Class.wizard =>
    {(Resource.arcaneRecovery, leveledResource lvl Resource.arcaneRecovery)}
  | DSpecify.sans Class.bard =>
    {(Resource.bardicInspiration, leveledResource lvl Resource.bardicInspiration)}
  | DSpecify.avec Class.wizard Subclass.schoolOfBladesinging =>
    {(Resource.bladesongPower, leveledResource lvl Resource.bladesongPower)}
  | DSpecify.avec Class.druid Subclass.circleOfTheStars =>
    { (Resource.cosmicOmen, leveledResource lvl Resource.cosmicOmen)
    , (Resource.starMap, leveledResource lvl Resource.starMap)
    }
  | DSpecify.avec Class.druid Subclass.circleOfTheSpores =>
    {(Resource.fungalInfestation, leveledResource lvl Resource.fungalInfestation)}
  | DSpecify.sans Class.paladin =>
    { (Resource.layOnHands, leveledResource lvl Resource.layOnHands)
    , (Resource.channelOath, leveledResource lvl Resource.channelOath)
    }
  | DSpecify.avec Class.druid Subclass.circleOfTheLand =>
    {(Resource.naturalRecovery, leveledResource lvl Resource.naturalRecovery)}
  | DSpecify.sans Class.barbarian =>
    {(Resource.rage, leveledResource lvl Resource.rage)}
  | DSpecify.sans Class.sorcerer =>
    {(Resource.sorceryPoint, leveledResource lvl Resource.sorceryPoint)}
  | DSpecify.avec Class.cleric Subclass.warDomain =>
    {(Resource.warPriest, leveledResource lvl Resource.warPriest)}
  | DSpecify.avec Class.ranger Subclass.swarmkeeper =>
    {(Resource.writhingCurrents, leveledResource lvl Resource.writhingCurrents)}
  | DSpecify.avec Class.fighter Subclass.arcaneArcher =>
    {(Resource.arcaneArrow, leveledResource lvl Resource.arcaneArrow)}
  | DSpecify.sans Class.cleric =>
    {(Resource.channelDivinity, leveledResource lvl Resource.channelDivinity)}
  | DSpecify.sans Class.monk =>
    {(Resource.kiPoint, leveledResource lvl Resource.kiPoint)}
  | DSpecify.avec Class.fighter Subclass.battleMaster =>
    {(Resource.superiorityDie, leveledResource lvl Resource.superiorityDie)}
  | DSpecify.avec Class.sorcerer Subclass.wildMagicSorcerer =>
    {(Resource.tidesOfChaos, 1)}
  | DSpecify.sans Class.druid =>
    {(Resource.wildShape, 2)}
  | DSpecify.avec Class.rogue Subclass.thief =>
    {(Resource.bonusAction, 1)}
  | DSpecify.sans Class.warlock =>
    warlockSpells lvl
  | _ => {}

structure ClassInfo (cls : Class) where
  level : Level
  subclass : Option (Subclass cls)

structure Character where
  name : String
  classes : Std.DTreeMap Class ClassInfo
  resources : Std.TreeMap Resource Nat

def effectiveSpellcastingLevel : Character -> Level :=
  fun char =>
    { level :=
      ( char.classes.foldl
        ( fun acc cls ci => match cls with
          | Class.barbarian => acc
          | Class.bard => acc + ci.level.level * 6
          | Class.cleric => acc + ci.level.level * 6
          | Class.druid => acc + ci.level.level * 6
          | Class.fighter => match ci.subclass with
            | Option.some Subclass.eldritchKnight => acc + ci.level.level * 2
            | _ => acc
          | Class.monk => acc
          | Class.paladin => acc + ci.level.level * 3
          | Class.ranger => acc + ci.level.level * 3
          | Class.rogue => match ci.subclass with
            | Option.some Subclass.arcaneTrickster => acc + ci.level.level * 2
            | _ => acc
          | Class.sorcerer => acc + ci.level.level * 6
          | Class.warlock => acc
          | Class.wizard => acc + ci.level.level * 6
        )
        0
      ) / 6
    }

def maxresources : Character -> Std.TreeMap Resource Int :=
  fun char =>
    Std.TreeMap.union
      ( char.classes.foldl
        ( fun acc cls ci =>
          Std.TreeMap.mergeWith
            (fun _ => Add.add)
            acc
            ( match ci.subclass with
              | Option.none => classResources ci.level (DSpecify.sans cls)
              | Option.some sub =>
                Std.TreeMap.union
                  (classResources ci.level (DSpecify.sans cls))
                  (classResources ci.level (DSpecify.avec cls sub))
            )
        )
        {(Resource.reaction, 1), (Resource.bonusAction, 1), (Resource.action, 1)}
      )
      (fullCaster (effectiveSpellcastingLevel char))

#eval
  ( maxresources
    { name := "Melanie"
    , classes :=
        Std.DTreeMap.insert
          (Std.DTreeMap.insert {} Class.warlock {level := {level := 6}, subclass := Option.some Subclass.theArchfey})
          Class.fighter
          {level := {level := 6}, subclass := Option.some Subclass.eldritchKnight}
    , resources := {}
    }
  )

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
