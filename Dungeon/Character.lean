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

def subclassLevel : Class -> Int :=
  fun cls => match cls with
  | .barbarian => 3
  | .bard => 3
  | .cleric => 1
  | .druid => 3
  | .fighter => 3
  | .monk => 3
  | .paladin => 1
  | .ranger => 3
  | .rogue => 3
  | .sorcerer => 1
  | .warlock => 1
  | .wizard => 2

inductive Subclass : Class -> Type where
  -- Barbarian
  | berserker : Subclass .barbarian
  | giant : Subclass .barbarian
  | wildMagicBarbarian : Subclass .barbarian
  | wildheart : Subclass .barbarian
  -- Bard
  | collegeOfGlamour : Subclass .bard
  | collegeOfLore : Subclass .bard
  | collegeOfSwords : Subclass .bard
  | collegeOfValour : Subclass .bard
  -- Cleric
  | deathDomain : Subclass .cleric
  | knowledgeDomain : Subclass .cleric
  | lifeDomain : Subclass .cleric
  | lightDomain : Subclass .cleric
  | natureDomain : Subclass .cleric
  | tempestDomain : Subclass .cleric
  | trickeryDomain : Subclass .cleric
  | warDomain : Subclass .cleric
  -- Druid
  | circleOfTheLand : Subclass .druid
  | circleOfTheMoon : Subclass .druid
  | circleOfTheSpores : Subclass .druid
  | circleOfTheStars : Subclass .druid
  -- Fighter
  | arcaneArcher : Subclass .fighter
  | battleMaster : Subclass .fighter
  | champion : Subclass .fighter
  | eldritchKnight : Subclass .fighter
  -- Monk
  | wayOfTheDrunkenMaster : Subclass .monk
  | wayOfTheFourElements : Subclass .monk
  | wayOfTheOpenHand : Subclass .monk
  | wayOfShadow : Subclass .monk
  -- Paladin
  | oathOfDevotion : Subclass .paladin
  | oathOfTheAncients : Subclass .paladin
  | oathOfTheCrown : Subclass .paladin
  | oathOfVengeance : Subclass .paladin
  | oathbreaker : Subclass .paladin
  -- Ranger
  | beastMaster : Subclass .ranger
  | gloomstalker : Subclass .ranger
  | hunter : Subclass .ranger
  | swarmkeeper : Subclass .ranger
  -- Rogue
  | arcaneTrickster : Subclass .rogue
  | assassin : Subclass .rogue
  | swashbuckler : Subclass .rogue
  | thief : Subclass .rogue
  -- Sorcerer
  | draconicBloodline : Subclass .sorcerer
  | shadowMagic : Subclass .sorcerer
  | stormSorcery : Subclass .sorcerer
  | wildMagicSorcerer : Subclass .sorcerer
  -- Warlock
  | theArchfey : Subclass .warlock
  | theFiend : Subclass .warlock
  | theGreatOldOne : Subclass .warlock
  | theHexblade : Subclass .warlock
  -- Wizard
  | schoolOfAbjuration : Subclass .wizard
  | schoolOfBladesinging : Subclass .wizard
  | schoolOfConjuration : Subclass .wizard
  | schoolOfDivination : Subclass .wizard
  | schoolOfEnchantment : Subclass .wizard
  | schoolOfEvocation : Subclass .wizard
  | schoolOfIllusion : Subclass .wizard
  | schoolOfNecromancy : Subclass .wizard
  | schoolOfTransmutation : Subclass .wizard
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

def leveledResource : Int -> Resource -> Int :=
  fun lv res => match res with
  | .reaction => 1
  | .bonusAction => 1
  | .action => 1
  | .spellSlot _ => 0
  | .warlockSpellSlot _ => 0
  | .arcaneRecovery => (lv + 1) / 2
  | .bardicInspiration =>
      if lv < 5 then 3 else if lv < 8 then 4 else 5
  | .bladesongPower =>
      if lv < 2 then 0 else if lv < 5 then 2 else if lv < 9 then 3 else 4
  | .cosmicOmen =>
      if lv < 6 then 0 else if lv < 9 then 3 else 4
  | .fungalInfestation =>
      if lv < 6 then 0 else 4
  | .layOnHands =>
      if lv < 4 then 4 else if lv < 10 then 5 else 6
  | .naturalRecovery => lv / 2
  | .rage =>
      if lv < 3 then 2 else if lv < 6 then 6 else if lv < 12 then 4 else 5
  | .sorceryPoint => if lv < 2 then 0 else lv
  | .starMap =>
      if lv < 2 then 0 else if lv < 5 then 2 else if lv < 9 then 3 else 4
  | .warPriest =>
      if lv < 5 then 3 else if lv < 8 then 4 else if lv < 11 then 5 else 6
  | .writhingCurrents =>
      if lv < 7 then 0 else if lv < 9 then 3 else 4
  | .arcaneArrow =>
      if lv < 3 then 0 else if lv < 7 then 4 else if lv < 10 then 7 else 10
  | .channelDivinity =>
      if lv < 6 then 1 else 2
  | .channelOath => 1
  | .kiPoint => lv + 1
  | .superiorityDie =>
      if lv < 7 then 4 else 5
  | .tidesOfChaos => 1
  | .wildShape => 2

inductive DSpecify (x : Type u) (y : x -> Type v) : Type (max u v) where
  | sans : x -> DSpecify x y
  | avec : (a : x) -> y a -> DSpecify x y

def fullCaster : Int -> Std.TreeMap Resource Int :=
  fun lvl => match lvl with
  | 1 => {(.spellSlot .i, 2)}
  | 2 => {(.spellSlot .i, 3)}
  | 3 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 2)
    }
  | 4 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    }
  | 5 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 2)
    }
  | 6 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    }
  | 7 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 1)
    }
  | 8 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 2)
    }
  | 9 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 3)
    , (.spellSlot .v, 1)
    }
  | 10 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 3)
    , (.spellSlot .v, 2)
    }
  | 11 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 3)
    , (.spellSlot .v, 2)
    , (.spellSlot .vi, 1)
    }
  | 12 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 3)
    , (.spellSlot .v, 2)
    , (.spellSlot .vi, 1)
    }
  | 13 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 3)
    , (.spellSlot .v, 2)
    , (.spellSlot .vi, 1)
    , (.spellSlot .vii, 1)
    }
  | 14 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 3)
    , (.spellSlot .v, 2)
    , (.spellSlot .vi, 1)
    , (.spellSlot .vii, 1)
    }
  | 15 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 3)
    , (.spellSlot .v, 2)
    , (.spellSlot .vi, 1)
    , (.spellSlot .vii, 1)
    , (.spellSlot .viii, 1)
    }
  | 16 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 3)
    , (.spellSlot .v, 2)
    , (.spellSlot .vi, 1)
    , (.spellSlot .vii, 1)
    , (.spellSlot .viii, 1)
    }
  | 17 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 3)
    , (.spellSlot .v, 2)
    , (.spellSlot .vi, 1)
    , (.spellSlot .vii, 1)
    , (.spellSlot .viii, 1)
    , (.spellSlot .ix, 1)
    }
  | 18 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 3)
    , (.spellSlot .v, 3)
    , (.spellSlot .vi, 1)
    , (.spellSlot .vii, 1)
    , (.spellSlot .viii, 1)
    , (.spellSlot .ix, 1)
    }
  | 19 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 3)
    , (.spellSlot .v, 3)
    , (.spellSlot .vi, 2)
    , (.spellSlot .vii, 1)
    , (.spellSlot .viii, 1)
    , (.spellSlot .ix, 1)
    }
  | 20 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 3)
    , (.spellSlot .v, 3)
    , (.spellSlot .vi, 2)
    , (.spellSlot .vii, 2)
    , (.spellSlot .viii, 1)
    , (.spellSlot .ix, 1)
    }
  | _ => {}

def halfCaster : Int -> Std.TreeMap Resource Int :=
  fun lvl => match lvl with
  | 1 => {}
  | 2 => {(.spellSlot .i, 2)}
  | 3 => {(.spellSlot .i, 3)}
  | 4 => {(.spellSlot .i, 3)}
  | 5 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 2)
    }
  | 6 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 2)
    }
  | 7 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    }
  | 8 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    }
  | 9 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 2)
    }
  | 10 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 2)
    }
  | 11 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    }
  | 12 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    }
  | 13 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 1)
    }
  | 14 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 1)
    }
  | 15 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 2)
    }
  | 16 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 2)
    }
  | 17 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 3)
    , (.spellSlot .v, 1)
    }
  | 18 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 3)
    , (.spellSlot .v, 1)
    }
  | 19 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 3)
    , (.spellSlot .v, 2)
    }
  | 20 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 3)
    , (.spellSlot .v, 2)
    }
  | _ => {}

def thirdCaster : Int -> Std.TreeMap Resource Int :=
  fun lvl => match lvl with
  | 1 => {}
  | 2 => {}
  | 3 => {(.spellSlot .i, 2)}
  | 4 => {(.spellSlot .i, 3)}
  | 5 => {(.spellSlot .i, 3)}
  | 6 => {(.spellSlot .i, 3)}
  | 7 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 2)
    }
  | 8 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 2)
    }
  | 9 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 2)
    }
  | 10 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    }
  | 11 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    }
  | 12 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    }
  | 13 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 2)
    }
  | 14 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 2)
    }
  | 15 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 2)
    }
  | 16 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    }
  | 17 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    }
  | 18 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    }
  | 19 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 1)
    }
  | 20 =>
    { (.spellSlot .i, 4)
    , (.spellSlot .ii, 3)
    , (.spellSlot .iii, 3)
    , (.spellSlot .iv, 1)
    }
  | _ => {}

def warlockSpells : Int -> Std.TreeMap Resource Int :=
  fun lvl => match lvl with
  | 1 => {(.warlockSpellSlot .i, 1)}
  | 2 => {(.warlockSpellSlot .i, 2)}
  | 3 => {(.warlockSpellSlot .ii, 2)}
  | 4 => {(.warlockSpellSlot .ii, 2)}
  | 5 => {(.warlockSpellSlot .iii, 2)}
  | 6 => {(.warlockSpellSlot .iii, 2)}
  | 7 => {(.warlockSpellSlot .iv, 2)}
  | 8 => {(.warlockSpellSlot .iv, 2)}
  | 9 => {(.warlockSpellSlot .v, 2)}
  | 10 => {(.warlockSpellSlot .v, 2)}
  | 11 => {(.warlockSpellSlot .v, 3)}
  | 12 => {(.warlockSpellSlot .v, 3)}
  | 13 => {(.warlockSpellSlot .v, 3)}
  | 14 => {(.warlockSpellSlot .v, 3)}
  | 15 => {(.warlockSpellSlot .v, 3)}
  | 16 => {(.warlockSpellSlot .v, 3)}
  | 17 => {(.warlockSpellSlot .v, 4)}
  | 18 => {(.warlockSpellSlot .v, 4)}
  | 19 => {(.warlockSpellSlot .v, 4)}
  | 20 => {(.warlockSpellSlot .v, 4)}
  | _ => {}

def leveledSpellSlots : Int -> DSpecify Class Subclass -> Std.TreeMap Resource Int :=
  fun lvl dspecify => match dspecify with
  | .sans .bard => fullCaster lvl
  | .sans .cleric => fullCaster lvl
  | .sans .druid => fullCaster lvl
  | .sans .paladin => halfCaster lvl
  | .sans .ranger => halfCaster lvl
  | .sans .sorcerer => fullCaster lvl
  | .sans .warlock => warlockSpells lvl
  | .sans .wizard => fullCaster lvl
  | .avec .rogue .arcaneTrickster => thirdCaster lvl
  | .avec .fighter .eldritchKnight => thirdCaster lvl
  | _ => {}

def classResources : Int -> DSpecify Class Subclass -> Std.TreeMap Resource Int :=
  fun lvl dspecify => match dspecify with
  | .sans .wizard =>
    {(.arcaneRecovery, leveledResource lvl .arcaneRecovery)}
  | .sans .bard =>
    {(.bardicInspiration, leveledResource lvl .bardicInspiration)}
  | .avec .wizard .schoolOfBladesinging =>
    {(.bladesongPower, leveledResource lvl .bladesongPower)}
  | .avec .druid .circleOfTheStars =>
    { (.cosmicOmen, leveledResource lvl .cosmicOmen)
    , (.starMap, leveledResource lvl .starMap)
    }
  | .avec .druid .circleOfTheSpores =>
    {(.fungalInfestation, leveledResource lvl .fungalInfestation)}
  | .sans .paladin =>
    { (.layOnHands, leveledResource lvl .layOnHands)
    , (.channelOath, leveledResource lvl .channelOath)
    }
  | .avec .druid .circleOfTheLand =>
    {(.naturalRecovery, leveledResource lvl .naturalRecovery)}
  | .sans .barbarian =>
    {(.rage, leveledResource lvl .rage)}
  | .sans .sorcerer =>
    {(.sorceryPoint, leveledResource lvl .sorceryPoint)}
  | .avec .cleric .warDomain =>
    {(.warPriest, leveledResource lvl .warPriest)}
  | .avec .ranger .swarmkeeper =>
    {(.writhingCurrents, leveledResource lvl .writhingCurrents)}
  | .avec .fighter .arcaneArcher =>
    {(.arcaneArrow, leveledResource lvl .arcaneArrow)}
  | .sans .cleric =>
    {(.channelDivinity, leveledResource lvl .channelDivinity)}
  | .sans .monk =>
    {(.kiPoint, leveledResource lvl .kiPoint)}
  | .avec .fighter .battleMaster =>
    {(.superiorityDie, leveledResource lvl .superiorityDie)}
  | .avec .sorcerer .wildMagicSorcerer =>
    {(.tidesOfChaos, 1)}
  | .sans .druid =>
    {(.wildShape, 2)}
  | .avec .rogue .thief =>
    {(.bonusAction, 1)}
  | .sans .warlock =>
    warlockSpells lvl
  | _ => {}

structure ClassInfo (cls : Class) where
  level : Int
  subclass : (level >= subclassLevel cls) -> Subclass cls

inductive Recharge where
  | onTurn
  | onShortRest
  | onLongRest
  deriving BEq, DecidableEq, Ord, Repr

def classResourceRecharge : Resource -> Recharge :=
  fun res => match res with
  | .reaction => .onTurn
  | .bonusAction => .onTurn
  | .action => .onTurn
  --
  | .spellSlot _ => .onLongRest
  | .warlockSpellSlot _ => .onShortRest
  --
  | .arcaneRecovery => .onLongRest
  | .bardicInspiration => .onLongRest
  | .bladesongPower => .onLongRest
  | .cosmicOmen => .onLongRest
  | .fungalInfestation => .onLongRest
  | .layOnHands => .onLongRest
  | .naturalRecovery => .onLongRest
  | .rage => .onLongRest
  | .sorceryPoint => .onLongRest
  | .starMap => .onLongRest
  | .warPriest => .onLongRest
  | .writhingCurrents => .onLongRest
  --
  | .arcaneArrow => .onShortRest
  | .channelDivinity => .onShortRest
  | .channelOath => .onShortRest
  | .kiPoint => .onShortRest
  | .superiorityDie => .onShortRest
  | .tidesOfChaos => .onShortRest
  | .wildShape => .onShortRest

inductive Ability where
  | strength
  | dexterity
  | constitution
  | intelligence
  | wisdom
  | charisma
  deriving BEq, DecidableEq, Ord, Repr

inductive Skill : Ability -> Type where
  | acrobatics : Skill .dexterity
  | animalHandling : Skill .wisdom
  | arcana : Skill .intelligence
  | athletics : Skill .strength
  | deception : Skill .charisma
  | history : Skill .intelligence
  | insight : Skill .wisdom
  | intimidation : Skill .charisma
  | investigation : Skill .intelligence
  | medicine : Skill .wisdom
  | nature : Skill .intelligence
  | perception : Skill .wisdom
  | performance : Skill .charisma
  | persuasion : Skill .charisma
  | religion : Skill .intelligence
  | sleightOfHand : Skill .dexterity
  | stealth : Skill .dexterity
  | survival : Skill .wisdom
  deriving BEq, DecidableEq, Ord, Repr

inductive Proficiency where
  | proficiency
  | expertise
  deriving BEq, DecidableEq, Ord, Repr

structure Skills (ab : Ability) where
  score : Nat
  proficiencies : Std.TreeMap (Skill ab) Proficiency

structure Character where
  name : String
  classes : Std.DTreeMap Class ClassInfo
  resources : Std.TreeMap Resource Nat
  skills : Std.DTreeMap Ability Skills

def characterLevel : Character -> Int :=
  fun char => char.classes.foldl (fun acc _ ci => acc + ci.level) 0

def effectiveSpellcastingLevel : Character -> Int :=
  fun char =>
    ( char.classes.foldl
      ( fun acc cls ci => match cls with
        | .barbarian => acc
        | .bard => acc + ci.level * 6
        | .cleric => acc + ci.level * 6
        | .druid => acc + ci.level * 6
        | .fighter => acc +
          if h : ci.level >= subclassLevel .fighter
            then match ci.subclass h with
            | .eldritchKnight => ci.level * 2
            | _ => 0
            else 0
        | .monk => acc
        | .paladin => acc + ci.level * 3
        | .ranger => acc + ci.level * 3
        | .rogue => acc +
          if h : ci.level >= subclassLevel .rogue
            then match ci.subclass h with
            | .arcaneTrickster => ci.level * 2
            | _ => 0
            else 0
        | .sorcerer => acc + ci.level * 6
        | .warlock => acc
        | .wizard => acc + ci.level * 6
      )
      0
    ) / 6

def maxresources : Character -> Std.TreeMap Resource Int :=
  fun char =>
    Std.TreeMap.union
      ( char.classes.foldl
        ( fun acc cls ci =>
          Std.TreeMap.mergeWith
            (fun _ => Add.add)
            acc
            ( if h : ci.level >= subclassLevel cls
                then match ci.subclass h with
                | sub =>
                  Std.TreeMap.union
                    (classResources ci.level (.sans cls))
                    (classResources ci.level (.avec cls sub))
                else classResources ci.level (.sans cls)
            )
        )
        {(.reaction, 1), (.bonusAction, 1), (.action, 1)}
      )
      (fullCaster (effectiveSpellcastingLevel char))

def melanie : Character where
  name := "Melanie"
  classes :=
    { ⟨.warlock, {level := 3, subclass := fun _ => .theArchfey}⟩
    , ⟨.rogue, {level := 1, subclass := nofun}⟩
    }
  resources := {}
  skills :=
    { ⟨.strength, {score := 13, proficiencies := {(.athletics, .proficiency)}}⟩
    , ⟨.dexterity, {score := 8, proficiencies := {(.stealth, .proficiency)}}⟩
    , ⟨.constitution, {score := 10, proficiencies := {}}⟩
    , ⟨.intelligence, {score := 17, proficiencies := {(.arcana, .expertise)}}⟩
    , ⟨.wisdom
      , { score := 15, proficiencies :=
          { (.perception, .proficiency)
          , (.insight, .proficiency)
          }
        }
      ⟩
    , ⟨.charisma, {score := 12, proficiencies := {(.deception, .proficiency)}}⟩
    }

#eval maxresources melanie
