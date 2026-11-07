import Std.Data.TreeMap.Basic

inductive Deity where
  | abbathor
  | aerdrieFaenya
  | akadi
  | asgorath
  | astilabor
  | amauntor
  | angharradh
  | annamAllfather
  | ao
  | arvoreen
  | asmodeus
  | auril
  | azuth
  | baervanWildwanderer
  | bahamut
  | bahgtru
  | bane
  | baphomet
  | baravarCloakshadow
  | berronarTruesilver
  | beshaba
  | bhaal
  | blibdoolpoolp
  | brandobaris
  | callarduranSmoothhands
  | clangeddinSilverbeard
  | chauntea
  | corellonLarethian
  | cyric
  | cyrrollalee
  | deepDuerra
  | deepSashelas
  | deneir
  | dugmarenBrightmantle
  | dumathoin
  | eadro
  | eilistraee
  | eldath
  | erevanIlesere
  | fenmarelMestarine
  | flandalSteelskin
  | finder
  | gaerdalIronhand
  | garagos
  | gargauth
  | garlGlittergold
  | garyx
  | ghaunadaur
  | gond
  | gormGulthyn
  | grolantor
  | grumbar
  | gruumsh
  | gwaeronWindstrom
  | haelaBrightaxe
  | hanaliCelanil
  | helm
  | hiatea
  | hlal
  | hoar
  | hruggek
  | iallanis
  | ibrandul
  | ilmater
  | ilneval
  | istishia
  | jergal
  | hiaransalee
  | kelemvor
  | kereska
  | kossuth
  | kurtulmak
  | labelasEnoreth
  | laduguer
  | lathander
  | laogzed
  | leira
  | lendys
  | lliira
  | lolth
  | loviatar
  | lurue
  | luthic
  | maglubiyet
  | malar
  | marthammorDuin
  | mask
  | memnor
  | merrshaulk
  | mielikki
  | milil
  | moradin
  | myrkul
  | mystra
  | nobanion
  | null
  | oghma
  | othea
  | persana
  | ramenos
  | redKnight
  | rillifaneRallathil
  | savras
  | segojanEarthcaller
  | sehanineMoonbow
  | sekolah
  | selvetarm
  | selûne
  | semuanya
  | shar
  | sharess
  | shargaas
  | sharindlar
  | sheelaPeryroyl
  | shevarash
  | shiallia
  | siamorphe
  | silvanus
  | skerrit
  | skoraeusStonebones
  | solonorThelandira
  | sseth
  | stronmaus
  | sune
  | surtr
  | talona
  | talos
  | tamara
  | task
  | tempus
  | thardHarr
  | thrym
  | tiamat
  | torm
  | trishina
  | tymora
  | tyr
  | ubtao
  | ulutiu
  | umberlee
  | urdlen
  | urogalan
  | uthgar
  | valkur
  | vaprak
  | velsharoon
  | vergadain
  | vhaeraun
  | vlaakith
  | waukeen
  | yondalla
  | yurtrus
  | zehir
  | zorquan
  deriving BEq, DecidableEq, Ord, Repr

inductive Creature where
  | aberration
  | beast
  | celestial
  | construct
  | dragon
  | elemental
  | fey
  | fiend
  | giant
  | humanoid
  | monstrosity
  | ooze
  | plant
  | undead
  deriving BEq, DecidableEq, Ord, Repr

inductive Race : Creature -> Type where
  -- Aberrations
  | aboleth : Race .aberration
  | beholder : Race .aberration
  | elderBrain : Race .aberration
  | flumph : Race .aberration
  | hookHorror : Race .aberration
  | illithid : Race .aberration
  | intellectDevourer : Race .aberration
  | kuoToa : Race .aberration
  | phasm : Race .aberration
  | yuanTi : Race .aberration
  -- Beasts
  | alligator : Race .beast
  | ant : Race .beast
  | ape : Race .beast
  | aurochs : Race .beast
  | baboon : Race .beast
  | badger : Race .beast
  | bat : Race .beast
  | bear : Race .beast
  | beaver : Race .beast
  | bird : Race .beast
  | boar : Race .beast
  | buffalo : Race .beast
  | camel : Race .beast
  | caribou : Race .beast
  | cat : Race .beast
  | centipede : Race .beast
  | cougar : Race .beast
  | cow : Race .beast
  | crab : Race .beast
  | crocodile : Race .beast
  | deer : Race .beast
  | dolphin : Race .beast
  | dog : Race .beast
  | donkey : Race .beast
  | elephant : Race .beast
  | elk : Race .beast
  | fish : Race .beast
  | fox : Race .beast
  | frog : Race .beast
  | goat : Race .beast
  | hare : Race .beast
  | horse : Race .beast
  | hyena : Race .beast
  | jackal : Race .beast
  | jaguar : Race .beast
  | leech : Race .beast
  | leopard : Race .beast
  | lion : Race .beast
  | lizard : Race .beast
  | mammoth : Race .beast
  | moose : Race .beast
  | mouse : Race .beast
  | nautilus : Race .beast
  | octopus : Race .beast
  | otter : Race .beast
  | ox : Race .beast
  | panther : Race .beast
  | pig : Race .beast
  | rat : Race .beast
  | rhinoceros : Race .beast
  | rothé : Race .beast
  | scorpion : Race .beast
  | sheep : Race .beast
  | snake : Race .beast
  | spider : Race .beast
  | tiger : Race .beast
  | toad : Race .beast
  | turtle : Race .beast
  | walrus : Race .beast
  | weasel : Race .beast
  | wolf : Race .beast
  | yak : Race .beast
  -- Celestials
  | angel : Race .celestial
  | guardinal : Race .celestial
  | hollyphant : Race .celestial
  | kiRin : Race .celestial
  | pegasus : Race .celestial
  | unicorn : Race .celestial
  -- Constructs
  | animatedArmour : Race .construct
  | flyingWeapon : Race .construct
  | golem : Race .construct
  | guardianPortrait : Race .construct
  | homunculus : Race .construct
  | inevitable : Race .construct
  | livingSpell : Race .construct
  | magen : Race .construct
  | modron : Race .construct
  | retriever : Race .construct
  | scarecrow : Race .construct
  | steelWatcher : Race .construct
  -- Dragons
  | chromaticDragon : Race .dragon
  | gemDragon : Race .dragon
  | kobold : Race .dragon
  | metallicDragon : Race .dragon
  -- Elementals
  | elemental : Race .elemental
  | genie : Race .elemental
  | mephit : Race .elemental
  | myrmidon : Race .elemental
  | phoenix : Race .elemental
  | salamander : Race .elemental
  | weird : Race .elemental
  -- Fey
  | almiraj : Race .fey
  | blinkDog : Race .fey
  | boggle : Race .fey
  | brownie : Race .fey
  | centaur : Race .fey
  | dryad : Race .fey
  | gremlin : Race .fey
  | hag : Race .fey
  | killmoulis : Race .fey
  | leprechaun : Race .fey
  | meenlock : Race .fey
  | naiad : Race .fey
  | nereid : Race .fey
  | nymph : Race .fey
  | oread : Race .fey
  | redcap : Race .fey
  | satyr : Race .fey
  | spriggan : Race .fey
  | sprite : Race .fey
  | willowisp : Race .fey
  -- Fiends
  | demodand : Race .fiend
  | demon : Race .fiend
  | devil : Race .fiend
  | hellhound : Race .fiend
  | hellsboar : Race .fiend
  | yugoloth : Race .fiend
  -- Giants
  | cloudGiant : Race .giant
  | cyclops : Race .giant
  | ettinGiant : Race .giant
  | fireGiant : Race .giant
  | frostGiant : Race .giant
  | hillGiant : Race .giant
  | ogre : Race .giant
  | stoneGiant : Race .giant
  | stormGiant : Race .giant
  | troll : Race .giant
  -- Humanoids
  | aarakocra : Race .humanoid
  | aasimar : Race .humanoid
  | bugbear : Race .humanoid
  | bullywug : Race .humanoid
  | changeling : Race .humanoid
  | dragonborn : Race .humanoid
  | dwarf : Race .humanoid
  | elf : Race .humanoid
  | firbolg : Race .humanoid
  | genasi : Race .humanoid
  | giff : Race .humanoid
  | gith : Race .humanoid
  | gnoll : Race .humanoid
  | gnome : Race .humanoid
  | goblin : Race .humanoid
  | halfElf : Race .humanoid
  | halfling : Race .humanoid
  | halfOrc : Race .humanoid
  | hobgoblin : Race .humanoid
  | human : Race .humanoid
  | lizardfolk : Race .humanoid
  | meazel : Race .humanoid
  | merfolk : Race .humanoid
  | orc : Race .humanoid
  | sahuagin : Race .humanoid
  | tabaxi : Race .humanoid
  | tiefling : Race .humanoid
  | tortle : Race .humanoid
  | triton : Race .humanoid
  | warforged : Race .humanoid
  | xvart : Race .humanoid
  -- Monstrosities
  | basilisk : Race .monstrosity
  | bulette : Race .monstrosity
  | chimera : Race .monstrosity
  | cockatrice : Race .monstrosity
  | displacer : Race .monstrosity
  | doppelganger : Race .monstrosity
  | drider : Race .monstrosity
  | ettercap : Race .monstrosity
  | gremishka : Race .monstrosity
  | griffon : Race .monstrosity
  | hydra : Race .monstrosity
  | kraken : Race .monstrosity
  | lamia : Race .monstrosity
  | lycanthrope : Race .monstrosity
  | manticore : Race .monstrosity
  | medusa : Race .monstrosity
  | mimic : Race .monstrosity
  | minotaur : Race .monstrosity
  | naga : Race .monstrosity
  | owlbear : Race .monstrosity
  | phaseSpider : Race .monstrosity
  | purpleWorm : Race .monstrosity
  | quaggoth : Race .monstrosity
  | sphinx : Race .monstrosity
  | tressym : Race .monstrosity
  | troglodyte : Race .monstrosity
  | worg : Race .monstrosity
  | yeti : Race .monstrosity
  -- Oozes
  | athcoid : Race .ooze
  | blackPudding : Race .ooze
  | greaseball : Race .ooze
  | greenSlime : Race .ooze
  | greyOoze : Race .ooze
  | ochreJelly : Race .ooze
  -- Plants
  | blight : Race .plant
  | campestri : Race .plant
  | kelpie : Race .plant
  | myconid : Race .plant
  | sporeServant : Race .plant
  | treant : Race .plant
  | vegepygmy : Race .plant
  | violetFungus : Race .plant
  | woodWoad : Race .plant
  -- Undead
  | banshee : Race .undead
  | bodak : Race .undead
  | boneclaw : Race .undead
  | brainJar : Race .undead
  | deathKnight : Race .undead
  | deathShepherd : Race .undead
  | deathTyrant : Race .undead
  | devourer : Race .undead
  | flameskull : Race .undead
  | ghost : Race .undead
  | ghoul : Race .undead
  | lich : Race .undead
  | mummy : Race .undead
  | nightwalker : Race .undead
  | poltergeist : Race .undead
  | revenant : Race .undead
  | shadow : Race .undead
  | skeleton : Race .undead
  | spectre : Race .undead
  | vampire : Race .undead
  | vampireSpawn : Race .undead
  | wight : Race .undead
  | zombie : Race .undead

inductive Subrace : Race c -> Type where
  -- Aberrations
  --
  | aboleth : Subrace .aboleth
  --
  | director : Subrace .beholder
  | elderOrb : Subrace .beholder
  | gauth : Subrace .beholder
  | gazer : Subrace .beholder
  | hiveMother : Subrace .beholder
  | observer : Subrace .beholder
  | spectator : Subrace .beholder
  --
  | elderBrain : Subrace .elderBrain
  | netherBrain : Subrace .elderBrain
  --
  | flumph : Subrace .flumph
  --
  | hookHorror : Subrace .hookHorror
  --
  | illithid : Subrace .illithid
  | ulitharid : Subrace .illithid
  --
  | intellectDevourer : Subrace .intellectDevourer
  --
  | kuoToa : Subrace .kuoToa
  --
  | phasm : Subrace .phasm
  --
  | pureblood : Subrace .yuanTi
  | halfblood : Subrace .yuanTi
  | abomination : Subrace .yuanTi
  | anathema : Subrace .yuanTi
  --
  -- Beasts
  --
  | alligator : Subrace .alligator
  --
  | ant : Subrace .ant
  | giantAnt : Subrace .ant
  --
  | ape : Subrace .ape
  --
  | aurochs : Subrace .aurochs
  --
  | baboon : Subrace .baboon
  --
  | badger : Subrace .baboon
  --
  | bat : Subrace .bat
  | giantBat : Subrace .bat
  --
  | brownBear : Subrace .bear
  | blackBear : Subrace .bear
  | caveBear : Subrace .bear
  | grizzlyBear : Subrace .bear
  --
  | beaver : Subrace .beaver
  --
  | albatross : Subrace .bird
  | axeBeak : Subrace .bird
  | blueJay : Subrace .bird
  | bluebird : Subrace .bird
  | canary : Subrace .bird
  | chicken : Subrace .bird
  | cormorant : Subrace .bird
  | crow : Subrace .bird
  | dove : Subrace .bird
  | eagle : Subrace .bird
  | giantEagle : Subrace .bird
  | emu : Subrace .bird
  | falcon : Subrace .bird
  | finch : Subrace .bird
  | flamingo : Subrace .bird
  | goose : Subrace .bird
  | grouse : Subrace .bird
  | hawk : Subrace .bird
  | heron : Subrace .bird
  | hummingbird : Subrace .bird
  | kestrel : Subrace .bird
  | kingfisher : Subrace .bird
  | macaw : Subrace .bird
  | magpie : Subrace .bird
  | osprey : Subrace .bird
  | ostrich : Subrace .bird
  | owl : Subrace .bird
  | giantOwl : Subrace .bird
  | parrot : Subrace .bird
  | peacock : Subrace .bird
  | penguin : Subrace .bird
  | pheasant : Subrace .bird
  | pigeon : Subrace .bird
  | puffin : Subrace .bird
  | quail : Subrace .bird
  | raven : Subrace .bird
  | giantRaven : Subrace .bird
  | roc : Subrace .bird
  | seagull : Subrace .bird
  | sparrow : Subrace .bird
  | starling : Subrace .bird
  | stork : Subrace .bird
  | toucan : Subrace .bird
  | turkey : Subrace .bird
  | vulture : Subrace .bird
  | giantVulture : Subrace .bird
  | warbler : Subrace .bird
  | woodpecker : Subrace .bird
  | wren : Subrace .bird
  --
  | boar : Subrace .boar
  | giantBoar : Subrace .boar
  --
  | buffalo : Subrace .buffalo
  --
  | camel : Subrace .camel
  --
  | caribou : Subrace .caribou
  --
  | cat : Subrace .cat
  --
  | centipede : Subrace .centipede
  --
  | cougar : Subrace .cougar
  --
  | cow : Subrace .cow
  --
  | crab : Subrace .crab
  | giantCrab : Subrace .crab
  --
  | crocodile : Subrace .crocodile
  --
  | deer : Subrace .deer
  --
  | dolphin : Subrace .dolphin
  --
  | bassetHound : Subrace .dog
  | bloodhound : Subrace .dog
  | dalmatian : Subrace .dog
  | mastiff : Subrace .dog
  | newfoundland : Subrace .dog
  | pug : Subrace .dog
  | spaniel : Subrace .dog
  --
  | donkey : Subrace .donkey
  --
  | elephant : Subrace .elephant
  --
  | elk : Subrace .elk
  --
  | barracuda : Subrace .fish
  | barbfish : Subrace .fish
  | blackjaw : Subrace .fish
  | bloatfish : Subrace .fish
  | brownfin : Subrace .fish
  | candlefish : Subrace .fish
  | catfish : Subrace .fish
  | dunchow : Subrace .fish
  | icetail : Subrace .fish
  | jewwelfish : Subrace .fish
  | hetfish : Subrace .fish
  | hogfish : Subrace .fish
  | manyfin : Subrace .fish
  | pike : Subrace .fish
  | piranha : Subrace .fish
  | quipper : Subrace .fish
  | salmon : Subrace .fish
  | sardine : Subrace .fish
  | shark : Subrace .fish
  | giantShark : Subrace .fish
  | steelhead : Subrace .fish
  | sunfish : Subrace .fish
  | trout : Subrace .fish
  --
  | fox : Subrace .fox
  --
  | frog : Subrace .frog
  --
  | goat : Subrace .goat
  | giantGoat : Subrace .goat
  | mountainGoat : Subrace .goat
  --
  | hare : Subrace .hare
  | giantHare : Subrace .hare
  --
  | heavyHorse : Subrace .horse
  | heavyWarhorse : Subrace .horse
  | lightHorse : Subrace .horse
  | lightWarhorse : Subrace .horse
  | pony : Subrace .horse
  | warPony : Subrace .horse
  --
  | hyena : Subrace .hyena
  --
  | jackal : Subrace .jackal
  --
  | jaguar : Subrace .jaguar
  --
  | leech : Subrace .leech
  | giantLeech : Subrace .leech
  --
  | leopard : Subrace .leopard
  --
  | lion : Subrace .lion
  --
  | bharang : Subrace .lizard
  | chameleon : Subrace .lizard
  | giantLizard : Subrace .lizard
  | minotaurLizard : Subrace .lizard
  | ridingLizard : Subrace .lizard
  | shockerLizard : Subrace .lizard
  | subterraneanLizard : Subrace .lizard
  --
  | mammoth : Subrace .mammoth
  --
  | moose : Subrace .moose
  --
  | mouse : Subrace .mouse
  --
  | nautilus : Subrace .nautilus
  --
  | octopus : Subrace .octopus
  | giantOctopus : Subrace .octopus
  --
  | otter : Subrace .otter
  --
  | ox : Subrace .ox
  --
  | panther : Subrace .panther
  --
  | pig : Subrace .pig
  --
  | rat : Subrace .rat
  --
  | rhinoceros : Subrace .rhinoceros
  --
  | rothé : Subrace .rothé
  --
  | scorpion : Subrace .scorpion
  --
  | sheep : Subrace .sheep
  | cragSheep : Subrace .sheep
  | mountainSheep : Subrace .sheep
  --
  | adder : Subrace .snake
  | asp : Subrace .snake
  | blackMamba : Subrace .snake
  | boomslang : Subrace .snake
  | cobra : Subrace .snake
  | giantCobra : Subrace .snake
  | constrictor : Subrace .snake
  | gaboon : Subrace .snake
  | jit : Subrace .snake
  | nightviper : Subrace .snake
  | rattlesnake : Subrace .snake
  | seaSnake : Subrace .snake
  | sewerm : Subrace .snake
  | viper : Subrace .snake
  --
  | spider : Subrace .spider
  | electricSpider : Subrace .spider
  | giantSpider : Subrace .spider
  | glowSpider : Subrace .spider
  | hairySpider : Subrace .spider
  | steeder : Subrace .spider
  | swordSpider : Subrace .spider
  --
  | tiger : Subrace .tiger
  --
  | toad : Subrace .toad
  --
  | turtle : Subrace .turtle
  | seaTurtle : Subrace .turtle
  --
  | walrus : Subrace .walrus
  --
  | weasel : Subrace .weasel
  --
  | wolf : Subrace .wolf
  | direWolf : Subrace .wolf
  --
  | yak : Subrace .yak
  --
  -- Celestials
  --
  | agathinon : Subrace .angel
  | astralDeva : Subrace .angel
  | monadicDeva : Subrace .angel
  | movanicDeva : Subrace .angel
  --
  | avoral : Subrace .guardinal
  | cervidal : Subrace .guardinal
  | equinal : Subrace .guardinal
  | leonal : Subrace .guardinal
  | lupinal : Subrace .guardinal
  | musteval : Subrace .guardinal
  | ursinal : Subrace .guardinal
  --
  | hollyphant : Subrace .hollyphant
  --
  | kiRin : Subrace .kiRin
  --
  | pegasus : Subrace .pegasus
  --
  | unicorn : Subrace .unicorn
  --
  -- Constructs
  --
  | animatedArmour : Subrace .animatedArmour
  --
  | flyingWeapon : Subrace .flyingWeapon
  --
  | bloodGolem : Subrace .golem
  | boneGolem : Subrace .golem
  | brainGolem : Subrace .golem
  | cadaverGolem : Subrace .golem
  | demonfleshGolem : Subrace .golem
  | dragonboneGolem : Subrace .golem
  | dragonfleshGolem : Subrace .golem
  | fangGolem : Subrace .golem
  | fleshGolem : Subrace .golem
  | ceramicGolem : Subrace .golem
  | clayGolem : Subrace .golem
  | clayWarrior : Subrace .golem
  | coalGolem : Subrace .golem
  | crystalGolem : Subrace .golem
  | geodeGolem : Subrace .golem
  | mudGolem : Subrace .golem
  | porcelainGolem : Subrace .golem
  | sandGolem : Subrace .golem
  | stoneGolem : Subrace .golem
  | adamantineGolem : Subrace .golem
  | brassGolem : Subrace .golem
  | bronzeGolem : Subrace .golem
  | chainGolem : Subrace .golem
  | ironGolem : Subrace .golem
  | mithralGolem : Subrace .golem
  | iceGolem : Subrace .golem
  --
  | guardianPortrait : Subrace .guardianPortrait
  --
  | homunculus : Subrace .homunculus
  --
  | inevitable : Subrace .inevitable
  --
  | livingSpell : Subrace .livingSpell
  --
  | demosMagen : Subrace .magen
  | galvenMagen : Subrace .magen
  | hypnosMagen : Subrace .magen
  --
  | monodrone : Subrace .modron
  | duodrone : Subrace .modron
  | tridrone : Subrace .modron
  | quadrone : Subrace .modron
  | pentadrone : Subrace .modron
  | decaton : Subrace .modron
  | nonaton : Subrace .modron
  | octon : Subrace .modron
  | septon : Subrace .modron
  | hexton : Subrace .modron
  --
  | retriever : Subrace .retriever
  --
  | scarecrow : Subrace .scarecrow
  --
  | steelWatcher : Subrace .steelWatcher
  | hellfireWatcher : Subrace .steelWatcher
  --
  -- Dragons
  --
  | redDragon : Subrace .chromaticDragon
  | blackDragon : Subrace .chromaticDragon
  | blueDragon : Subrace .chromaticDragon
  | greenDragon : Subrace .chromaticDragon
  | whiteDragon : Subrace .chromaticDragon
  | brownDragon : Subrace .chromaticDragon
  | purpleDragon : Subrace .chromaticDragon
  | greyDragon : Subrace .chromaticDragon
  | yellowDragon : Subrace .chromaticDragon
  | pinkDragon : Subrace .chromaticDragon
  --
  | amethystDragon : Subrace .gemDragon
  | emeraldDragon : Subrace .gemDragon
  | sapphireDragon : Subrace .gemDragon
  | topazDragon : Subrace .gemDragon
  | crystalDragon : Subrace .gemDragon
  | obsidianDragon : Subrace .gemDragon
  | beljurilDragon : Subrace .gemDragon
  | rubyDragon : Subrace .gemDragon
  | amberDragon : Subrace .gemDragon
  --
  | kobold : Subrace .kobold
  --
  | goldDragon : Subrace .metallicDragon
  | silverDragon : Subrace .metallicDragon
  | bronzeDragon : Subrace .metallicDragon
  | copperDragon : Subrace .metallicDragon
  | brassDragon : Subrace .metallicDragon
  | mercuryDragon : Subrace .metallicDragon
  | electrumDragon : Subrace .metallicDragon
  | ironDragon : Subrace .metallicDragon
  | steelDragon : Subrace .metallicDragon
  | platinumDragon : Subrace .metallicDragon
  --
  -- Elementals
  --
  | airElemental : Subrace .elemental
  | earthElemental : Subrace .elemental
  | fireElemental : Subrace .elemental
  | waterElemental : Subrace .elemental
  --
  | dao : Subrace .genie
  | djinni : Subrace .genie
  | efreeti : Subrace .genie
  | marid : Subrace .genie
  | jann : Subrace .genie
  | khayal : Subrace .genie
  --
  | airMephit : Subrace .mephit
  | earthMephit : Subrace .mephit
  | fireMephit : Subrace .mephit
  | waterMephit : Subrace .mephit
  | greaseMephit : Subrace .mephit
  | magmaMephit : Subrace .mephit
  | mudMephit : Subrace .mephit
  | stoneMephit : Subrace .mephit
  --
  | airMyrmidon : Subrace .myrmidon
  | earthMyrmidon : Subrace .myrmidon
  | fireMyrmidon : Subrace .myrmidon
  | waterMyrmidon : Subrace .myrmidon
  --
  | phoenix : Subrace .phoenix
  --
  | fireSnake : Subrace .salamander
  | flamebrother : Subrace .salamander
  | salamander : Subrace .salamander
  | nobleSalamander : Subrace .salamander
  --
  | airWeird : Subrace .weird
  | earthWeird : Subrace .weird
  | fireWeird : Subrace .weird
  | waterWeird : Subrace .weird
  --
  -- Fey
  --
  | almiraj : Subrace .almiraj
  --
  | blinkDog : Subrace .blinkDog
  --
  | boggle : Subrace .boggle
  --
  | brownie : Subrace .brownie
  --
  | centaur : Subrace .centaur
  --
  | dryad : Subrace .dryad
  --
  | gremlin : Subrace .gremlin
  --
  | annis : Subrace .hag
  | bheur : Subrace .hag
  | bogHag : Subrace .hag
  | greenHag : Subrace .hag
  | hagspawn : Subrace .hag
  | nightHag : Subrace .hag
  | seaHag : Subrace .hag
  --
  | killmoulis : Subrace .killmoulis
  --
  | leprechaun : Subrace .leprechaun
  --
  | meenlock : Subrace .meenlock
  --
  | naiad : Subrace .naiad
  --
  | nereid : Subrace .nereid
  --
  | nymph : Subrace .nymph
  --
  | oread : Subrace .oread
  --
  | redcap : Subrace .redcap
  | madcap : Subrace .redcap
  --
  | satyr : Subrace .satyr
  --
  | spriggan : Subrace .spriggan
  --
  | willowisp : Subrace .willowisp
  --
  -- Fiends
  --
  | farastu : Subrace .demodand
  | kelubar : Subrace .demodand
  | shator : Subrace .demodand
  --
  | balor : Subrace .demon
  | obyrith : Subrace .demon
  | tanar'ri : Subrace .demon
  | loumara : Subrace .demon
  | ghour : Subrace .demon
  | goristro : Subrace .demon
  | quasit : Subrace .demon
  | yochlol : Subrace .demon
  --
  | abishai : Subrace .devil
  | advespa : Subrace .devil
  | amnizu : Subrace .devil
  | barbazu : Subrace .devil
  | brachina : Subrace .devil
  | erinyes : Subrace .devil
  | imp : Subrace .devil
  | lemure : Subrace .devil
  | merregon : Subrace .devil
  | narzugon : Subrace .devil
  | nupperibo : Subrace .devil
  | orthon : Subrace .devil
  | pitFiend : Subrace .devil
  --
  | hellhound : Subrace .hellhound
  --
  | hellsboar : Subrace .hellsboar
  --
  | battleloth : Subrace .yugoloth
  | canoloth : Subrace .yugoloth
  | skeroloth : Subrace .yugoloth
  | voor : Subrace .yugoloth
  | mezzoloth : Subrace .yugoloth
  | dhergoloth : Subrace .yugoloth
  | piscoloth : Subrace .yugoloth
  | hydroloth : Subrace .yugoloth
  | yagnoloth : Subrace .yugoloth
  | merrenoloth : Subrace .yugoloth
  | nycaloth : Subrace .yugoloth
  | arcanaloth : Subrace .yugoloth
  | ultroloth : Subrace .yugoloth
  | altraloth : Subrace .yugoloth
  | baernoloth : Subrace .yugoloth
  | oinoloth : Subrace .yugoloth
  --
  -- Giants
  --
  | cloudGiant : Subrace .cloudGiant
  | cyclops : Subrace .cyclops
  | ettinGiant : Subrace .ettinGiant
  | fireGiant : Subrace .fireGiant
  | frostGiant : Subrace .frostGiant
  | hillGiant : Subrace .hillGiant
  | ogre : Subrace .ogre
  | stoneGiant : Subrace .stoneGiant
  | stormGiant : Subrace .stormGiant
  | troll : Subrace .troll
  --
  -- Humanoids
  --
  | aarakocra : Subrace .aarakocra
  --
  | fallenAasimar : Subrace .aasimar
  | protectorAasimar : Subrace .aasimar
  | scourgeAasimar : Subrace .aasimar
  --
  | bugbear : Subrace .bugbear
  --
  | bullywug : Subrace .bullywug
  --
  | changeling : Subrace .changeling
  --
  | blackDragonborn : Subrace .dragonborn
  | blueDragonborn : Subrace .dragonborn
  | brassDragonborn : Subrace .dragonborn
  | bronzeDragonborn : Subrace .dragonborn
  | copperDragonborn : Subrace .dragonborn
  | goldDragonborn : Subrace .dragonborn
  | greenDragonborn : Subrace .dragonborn
  | redDragonborn : Subrace .dragonborn
  | silverDragonborn : Subrace .dragonborn
  | whiteDragonborn : Subrace .dragonborn
  | amethystDragonborn : Subrace .dragonborn
  | crystalDragonborn : Subrace .dragonborn
  | emeraldDragonborn : Subrace .dragonborn
  | sapphireDragonborn : Subrace .dragonborn
  | topazDragonborn : Subrace .dragonborn
  --
  | arcticDwarf : Subrace .dwarf
  | goldDwarf : Subrace .dwarf
  | duergar : Subrace .dwarf
  | shieldDwarf : Subrace .dwarf
  | urdunnir : Subrace .dwarf
  --
  | sunElf : Subrace .elf
  | moonElf : Subrace .elf
  | starElf : Subrace .elf
  | woodElf : Subrace .elf
  | udadrow : Subrace .elf
  | aevendrow : Subrace .elf
  | lorendrow : Subrace .elf
  | snowElf : Subrace .elf
  --
  | firbolg : Subrace .firbolg
  --
  | airGenasi : Subrace .genasi
  | earthGenasi : Subrace .genasi
  | fireGenasi : Subrace .genasi
  | waterGenasi : Subrace .genasi
  --
  | giff : Subrace .giff
  --
  | githyanki : Subrace .gith
  | githzerai : Subrace .gith
  --
  | gnoll : Subrace .gnoll
  --
  | forestGnome : Subrace .gnome
  | rockGnome : Subrace .gnome
  | deepGnome : Subrace .gnome
  --
  | batiri : Subrace .goblin
  | dekanter : Subrace .goblin
  | nilbog : Subrace .goblin
  --
  | sunHalfElf : Subrace .halfElf
  | moonHalfElf : Subrace .halfElf
  | starHalfElf : Subrace .halfElf
  | woodHalfElf : Subrace .halfElf
  | drowHalfElf : Subrace .halfElf
  --
  | ghostwiseHalfling : Subrace .halfling
  | lightfootHalfling : Subrace .halfling
  | strongheartHalfling : Subrace .halfling
  --
  | halfOrc : Subrace .halfOrc
  --
  | hobgoblin : Subrace .hobgoblin
  --
  | human : Subrace .human
  --
  | lizardfolk : Subrace .lizardfolk
  --
  | meazel : Subrace .meazel
  --
  | merfolk : Subrace .merfolk
  --
  | mountainOrc : Subrace .orc
  | greyOrc : Subrace .orc
  | orog : Subrace .orc
  | ogrillon : Subrace .orc
  --
  | sahuagin : Subrace .sahuagin
  --
  | tabaxi : Subrace .tabaxi
  --
  | infernal : Subrace .tiefling
  | abyssal : Subrace .tiefling
  | cthonic : Subrace .tiefling
  --
  | tortle : Subrace .tortle
  --
  | triton : Subrace .triton
  --
  | warforged : Subrace .warforged
  --
  | xvart : Subrace .xvart
  --
  -- Monstrosities
  --
  | basilisk : Subrace .basilisk
  --
  | bulette : Subrace .bulette
  --
  | chimera : Subrace .chimera
  --
  | cockatrice : Subrace .cockatrice
  --
  | displacer : Subrace .displacer
  --
  | doppelganger : Subrace .doppelganger
  --
  | drider : Subrace .drider
  --
  | ettercap : Subrace .ettercap
  --
  | gremishka : Subrace .gremishka
  --
  | griffon : Subrace .griffon
  --
  | hydra : Subrace .hydra
  --
  | kraken : Subrace .kraken
  --
  | lamia : Subrace .lamia
  --
  | wereape : Subrace .lycanthrope
  | werebadger : Subrace .lycanthrope
  | werebat : Subrace .lycanthrope
  | werebear : Subrace .lycanthrope
  | wereboar : Subrace .lycanthrope
  | werebison : Subrace .lycanthrope
  | werecat : Subrace .lycanthrope
  | werecougar : Subrace .lycanthrope
  | werecrocodile : Subrace .lycanthrope
  | weredog : Subrace .lycanthrope
  | weredolphin : Subrace .lycanthrope
  | werefox : Subrace .lycanthrope
  | werehyena : Subrace .lycanthrope
  | werejackal : Subrace .lycanthrope
  | werejaguar : Subrace .lycanthrope
  | wereleopard : Subrace .lycanthrope
  | werelion : Subrace .lycanthrope
  | weremole : Subrace .lycanthrope
  | wereowl : Subrace .lycanthrope
  | werepanther : Subrace .lycanthrope
  | wererat : Subrace .lycanthrope
  | wereraven : Subrace .lycanthrope
  | wereseal : Subrace .lycanthrope
  | wereserpent : Subrace .lycanthrope
  | wereshark : Subrace .lycanthrope
  | werespider : Subrace .lycanthrope
  | weretiger : Subrace .lycanthrope
  | wereweasel : Subrace .lycanthrope
  | werewolf : Subrace .lycanthrope
  | werewolverine : Subrace .lycanthrope
  --
  | manticore : Subrace .manticore
  --
  | medusa : Subrace .medusa
  --
  | mimic : Subrace .mimic
  --
  | minotaur : Subrace .minotaur
  --
  | darkNaga : Subrace .naga
  | guardianNaga : Subrace .naga
  | iridescentNaga : Subrace .naga
  | spiritNaga : Subrace .naga
  | waterNaga : Subrace .naga
  --
  | owlbear : Subrace .owlbear
  --
  | phaseSpider : Subrace .phaseSpider
  --
  | purpleWorm : Subrace .purpleWorm
  --
  | quaggoth : Subrace .quaggoth
  --
  | sphinx : Subrace .sphinx
  --
  | tressym : Subrace .tressym
  --
  | troglodyte : Subrace .troglodyte
  --
  | worg : Subrace .worg
  --
  | yeti : Subrace .yeti
  --
  -- Oozes
  --
  | athcoid : Subrace .athcoid
  | blackPudding : Subrace .blackPudding
  | greaseball : Subrace .greaseball
  | greenSlime : Subrace .greenSlime
  | greyOoze : Subrace .greyOoze
  | ochreJelly : Subrace .ochreJelly
  --
  -- Plants
  --
  | needleBlight : Subrace .blight
  | treeBlight : Subrace .blight
  | twigBlight : Subrace .blight
  | vineBlight : Subrace .blight
  --
  | campestri : Subrace .campestri
  --
  | kelpie : Subrace .kelpie
  --
  | myconid : Subrace .myconid
  --
  | spreServant : Subrace .sporeServant
  --
  | rimwood : Subrace .treant
  | midwood : Subrace .treant
  | starwood : Subrace .treant
  --
  | vegempygmy : Subrace .vegepygmy
  --
  | violetFungus : Subrace .violetFungus
  --
  | woodWoad : Subrace .woodWoad
  --
  -- Undead
  --
  | banshee : Subrace .banshee
  | bodak : Subrace .bodak
  | boneclaw : Subrace .boneclaw
  | brainJar : Subrace .brainJar
  | deathKnight : Subrace .deathKnight
  | deathShepherd : Subrace .deathShepherd
  | deathTyrant : Subrace .deathTyrant
  | devourer : Subrace .devourer
  | flameskull : Subrace .flameskull
  | ghost : Subrace .ghost
  | ghoul : Subrace .ghoul
  | lich : Subrace .lich
  | mummy : Subrace .mummy
  | nightwalker : Subrace .nightwalker
  | poltergeist : Subrace .poltergeist
  | revenant : Subrace .revenant
  | shadow : Subrace .shadow
  | skeleton : Subrace .skeleton
  | spectre : Subrace .spectre
  | vampire : Subrace .vampire
  | vampireSpawn : Subrace .vampireSpawn
  | wight : Subrace .wight
  | zombie : Subrace .zombie

inductive Class where
  | barbarian
  | bard
  | cleric : Deity -> Class
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
  | .cleric _ => 1
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
  | pathOfTheAncestralGuardian : Subclass .barbarian
  | pathOfTheBattlerager : Subclass .barbarian
  | pathOfTheBeast : Subclass .barbarian
  | pathOfTheBerserker : Subclass .barbarian
  | pathOfTheGiant : Subclass .barbarian
  | pathOfTheStormHerald : Subclass .barbarian
  | pathOfTheTotemWarrior : Subclass .barbarian
  | pathOfTheWildheart : Subclass .barbarian
  | pathOfWildMagic : Subclass .barbarian
  | pathOfTheZealot : Subclass .barbarian
  -- Bard
  | collegeOfCreation : Subclass .bard
  | collegeOfEloquence : Subclass .bard
  | collegeOfGlamour : Subclass .bard
  | collegeOfLore : Subclass .bard
  | collegeOfSpirits : Subclass .bard
  | collegeOfSwords : Subclass .bard
  | collegeOfValour : Subclass .bard
  | collegeOfWhispers : Subclass .bard
  -- Cleric
  | arcanaDomain : Subclass (.cleric deity)
  | deathDomain : Subclass (.cleric deity)
  | forgeDomain : Subclass (.cleric deity)
  | graveDomain : Subclass (.cleric deity)
  | knowledgeDomain : Subclass (.cleric deity)
  | lifeDomain : Subclass (.cleric deity)
  | lightDomain : Subclass (.cleric deity)
  | natureDomain : Subclass (.cleric deity)
  | orderDomain : Subclass (.cleric deity)
  | peaceDomain : Subclass (.cleric deity)
  | tempestDomain : Subclass (.cleric deity)
  | trickeryDomain : Subclass (.cleric deity)
  | twilightDomain : Subclass (.cleric deity)
  | warDomain : Subclass (.cleric deity)
  -- Druid
  | circleOfDreams : Subclass .druid
  | circleOfTheLand : Subclass .druid
  | circleOfTheMoon : Subclass .druid
  | circleOfTheShepherd : Subclass .druid
  | circleOfTheSpores : Subclass .druid
  | circleOfTheStars : Subclass .druid
  | circleOfWildfire : Subclass .druid
  -- Fighter
  | arcaneArcher : Subclass .fighter
  | battleMaster : Subclass .fighter
  | cavalier : Subclass .fighter
  | champion : Subclass .fighter
  | echoKnight : Subclass .fighter
  | eldritchKnight : Subclass .fighter
  | psiWarrior : Subclass .fighter
  | purpleDragonKnight : Subclass .fighter
  | runeKnight : Subclass .fighter
  | samurai : Subclass .fighter
  -- Monk
  | wayOfTheAscendantDragon : Subclass .monk
  | wayOfTheAstralSelf : Subclass .monk
  | wayOfTheDrunkenMaster : Subclass .monk
  | wayOfTheFourElements : Subclass .monk
  | wayOfTheKensei : Subclass .monk
  | wayOfTheLongDeath : Subclass .monk
  | wayOfMercy : Subclass .monk
  | wayOfTheOpenHand : Subclass .monk
  | wayOfShadow : Subclass .monk
  | wayOfTheSunSoul : Subclass .monk
  -- Paladin
  | oathOfTheAncients : Subclass .paladin
  | oathOfConquest : Subclass .paladin
  | oathOfTheCrown : Subclass .paladin
  | oathOfDevotion : Subclass .paladin
  | oathOfGlory : Subclass .paladin
  | oathOfRedemption : Subclass .paladin
  | oathOfVengeance : Subclass .paladin
  | oathOfTheWatchers : Subclass .paladin
  | oathbreaker : Subclass .paladin
  -- Ranger
  | beastMaster : Subclass .ranger
  | drakewarden : Subclass .ranger
  | feyWanderer : Subclass .ranger
  | gloomstalker : Subclass .ranger
  | horizonWalker : Subclass .ranger
  | hunter : Subclass .ranger
  | monsterSlayer : Subclass .ranger
  | swarmkeeper : Subclass .ranger
  -- Rogue
  | arcaneTrickster : Subclass .rogue
  | assassin : Subclass .rogue
  | inquisitive : Subclass .rogue
  | mastermind : Subclass .rogue
  | phantom : Subclass .rogue
  | scout : Subclass .rogue
  | soulknife : Subclass .rogue
  | swashbuckler : Subclass .rogue
  | thief : Subclass .rogue
  -- Sorcerer
  | aberrantMind : Subclass .sorcerer
  | clockworkSoul : Subclass .sorcerer
  | divineSoul : Subclass .sorcerer
  | draconicBloodline : Subclass .sorcerer
  | lunarSorcery : Subclass .sorcerer
  | shadowMagic : Subclass .sorcerer
  | stormSorcery : Subclass .sorcerer
  | wildMagic : Subclass .sorcerer
  -- Warlock
  | theArchfey : Subclass .warlock
  | theCelestial : Subclass .warlock
  | theFathomless : Subclass .warlock
  | theFiend : Subclass .warlock
  | theGenie : Subclass .warlock
  | theGreatOldOne : Subclass .warlock
  | theHexblade : Subclass .warlock
  | theUndead : Subclass .warlock
  | theUndying : Subclass .warlock
  -- Wizard
  | orderOfScribes : Subclass .wizard
  | schoolOfAbjuration : Subclass .wizard
  | schoolOfBladesinging : Subclass .wizard
  | schoolOfChonurgy : Subclass .wizard
  | schoolOfConjuration : Subclass .wizard
  | schoolOfDivination : Subclass .wizard
  | schoolOfEnchantment : Subclass .wizard
  | schoolOfEvocation : Subclass .wizard
  | schoolOfGraviturgy : Subclass .wizard
  | schoolOfIllusion : Subclass .wizard
  | schoolOfNecromancy : Subclass .wizard
  | schoolOfTransmutation : Subclass .wizard
  | schoolOfWarMagic : Subclass .wizard
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
  | lucky
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
  | .lucky => 3
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
  | .sans (.cleric _) => fullCaster lvl
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
  | .avec (.cleric _) .warDomain =>
    {(.warPriest, leveledResource lvl .warPriest)}
  | .avec .ranger .swarmkeeper =>
    {(.writhingCurrents, leveledResource lvl .writhingCurrents)}
  | .avec .fighter .arcaneArcher =>
    {(.arcaneArrow, leveledResource lvl .arcaneArrow)}
  | .sans (.cleric _) =>
    {(.channelDivinity, leveledResource lvl .channelDivinity)}
  | .sans .monk =>
    {(.kiPoint, leveledResource lvl .kiPoint)}
  | .avec .fighter .battleMaster =>
    {(.superiorityDie, leveledResource lvl .superiorityDie)}
  | .avec .sorcerer .wildMagic =>
    {(.tidesOfChaos, 1)}
  | .sans .druid =>
    {(.wildShape, 2)}
  | .avec .rogue .thief =>
    {(.bonusAction, 1)}
  | .sans .warlock =>
    warlockSpells lvl
  | _ => {}

structure ClassInfo (cls : Class) where
  level : Nat
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
  | .lucky => .onLongRest
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
  modifier : Nat
  proficiencies : Std.TreeMap (Skill ab) Proficiency

inductive Damage where
  | bludgeoning
  | piercing
  | slashing
  | acid
  | cold
  | fire
  | force
  | lightning
  | necrotic
  | poison
  | psychic
  | radiant
  | thunder
  deriving BEq, DecidableEq, Ord, Repr

inductive DamageSource where
  | nonmagical
  | magical
  deriving BEq, DecidableEq, Ord, Repr

inductive Resistance where
  | vulnerability
  | resistance : DamageSource -> Resistance
  | immunity : DamageSource -> Resistance
  deriving BEq, DecidableEq, Ord, Repr

inductive Feat where
  | abilityImprovement : Ability -> Ability -> Feat
  | actor
  | alert
  | athleteStrength
  | athleteDexterity
  | charger
  | crossbowExpert
  | defensiveDuellist
  | dualWielder
  | dungeonDelver
  | durable
  | elementalAdeptAcid
  | elementalAdeptCold
  | elementalAdeptFire
  | elementalAdeptLightning
  | elementalAdeptThunder
  | greatWeaponMaster
  | heavilyArmoured
  | heavyArmourMaster
  | lightlyArmouredStrength
  | lightlyArmouredDexterity
  | lucky
  | mageSlayer
  | magicInitiateBard
  | magicInitiateCleric
  | magicInitiateDruid
  | magicInitiateSorcerer
  | magicInitiateWarlock
  | magicInitiateWizard
  | martialAdept
  | mediumArmourMaster
  | mobile
  | moderatelyArmouredStrength
  | moderatelyArmouredDexterity
  | performer
  | polearmMaster
  | resilient : Ability -> Feat
  | ritualCaster
  | savageAttacker
  | sentinel
  | sharpshooter
  | shieldMaster
  | skilled : Skill a -> Skill b -> Skill c -> Feat
  | spellSniper
  | tavernBrawlerStrength
  | tavernBrawlerConstitution
  | tough
  | warCaster
  | weaponMasterStrength
  | weaponMasterDexterity

structure Character where
  name : String
  creature : Creature
  race : Race creature
  subrace : Subrace race
  classes : Std.DTreeMap Class ClassInfo
  classesNotEmpty : ∃ (x : Class), x ∈ classes
  resources : Std.TreeMap Resource Nat
  skills : Std.DTreeMap Ability Skills
  skillsHasAllAbilities : ∀ (x : Ability), x ∈ skills
  feats : List Feat

def characterLevel : Character -> Int :=
  fun char => char.classes.foldl (fun acc _ ci => acc + ci.level) 0

def effectiveSpellcastingLevel : Character -> Int :=
  fun char =>
    char.classes.foldl
      ( fun acc cls ci => match cls with
        | .barbarian => acc
        | .bard => acc + ci.level * 6
        | .cleric _ => acc + ci.level * 6
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
      / 6

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

def scores : Character -> Std.TreeMap Ability Nat :=
  fun char =>
    let base :=
          char.skills.foldl
            (fun acc ab skills => Std.TreeMap.union acc {(ab, skills.score + skills.modifier)})
            {}
    let fromFeats :=
          char.feats.foldl
            ( fun acc feat =>
              Std.TreeMap.union
                acc
                ( match feat with
                  | .abilityImprovement ab0 ab1 =>
                    Std.TreeMap.mergeWith
                      (fun _ => Add.add)
                      {(ab0, 1)}
                      {(ab1, 1)}
                  | .actor => {(.charisma, 1)}
                  | .alert => {}
                  | .athleteStrength => {(.strength, 1)}
                  | .athleteDexterity => {(.dexterity, 1)}
                  | .charger => {}
                  | .crossbowExpert => {}
                  | .defensiveDuellist => {}
                  | .dualWielder => {}
                  | .dungeonDelver => {}
                  | .durable => {(.constitution, 1)}
                  | .elementalAdeptAcid => {}
                  | .elementalAdeptCold => {}
                  | .elementalAdeptFire => {}
                  | .elementalAdeptLightning => {}
                  | .elementalAdeptThunder => {}
                  | .greatWeaponMaster => {}
                  | .heavilyArmoured => {(.strength, 1)}
                  | .heavyArmourMaster => {(.strength, 1)}
                  | .lightlyArmouredStrength => {(.strength, 1)}
                  | .lightlyArmouredDexterity => {(.dexterity, 1)}
                  | .lucky => {}
                  | .mageSlayer => {}
                  | .magicInitiateBard => {}
                  | .magicInitiateCleric => {}
                  | .magicInitiateDruid => {}
                  | .magicInitiateSorcerer => {}
                  | .magicInitiateWarlock => {}
                  | .magicInitiateWizard => {}
                  | .martialAdept => {}
                  | .mediumArmourMaster => {}
                  | .mobile => {}
                  | .moderatelyArmouredStrength => {(.strength, 1)}
                  | .moderatelyArmouredDexterity => {(.dexterity, 1)}
                  | .performer => {(.charisma, 1)}
                  | .polearmMaster => {}
                  | .resilient ab => {(ab, 1)}
                  | .ritualCaster => {}
                  | .savageAttacker => {}
                  | .sentinel => {}
                  | .sharpshooter => {}
                  | .shieldMaster => {}
                  | .skilled _ _ _ => {}
                  | .spellSniper => {}
                  | .tavernBrawlerStrength => {(.strength, 1)}
                  | .tavernBrawlerConstitution => {(.constitution, 1)}
                  | .tough => {}
                  | .warCaster => {}
                  | .weaponMasterStrength => {(.strength, 1)}
                  | .weaponMasterDexterity => {(.dexterity, 1)}
                )
            )
            {}
  Std.TreeMap.mergeWith
      (fun _ score0 score1 => min 20 (score0 + score1))
      base
      fromFeats

def carryingCapacity : Character -> Int :=
  fun char =>
    let strength := ((scores char).get? .strength).getD 0
    (40 + 10 * strength)

def melanie : Character where
  name := "Melanie"
  creature := .humanoid
  race := .human
  subrace := .human
  classes :=
    { ⟨.warlock, {level := 3, subclass := fun _ => .theArchfey}⟩
    , ⟨.rogue, {level := 1, subclass := nofun}⟩
    }
  classesNotEmpty := by apply Exists.intro .warlock; trivial
  resources := {}
  skills :=
    { ⟨.strength, {score := 13, modifier := 0, proficiencies := {(.athletics, .proficiency)}}⟩
    , ⟨.dexterity, {score := 8, modifier := 0, proficiencies := {(.stealth, .proficiency)}}⟩
    , ⟨.constitution, {score := 10, modifier := 0, proficiencies := {}}⟩
    , ⟨.intelligence, {score := 15, modifier := 2, proficiencies := {(.arcana, .expertise)}}⟩
    , ⟨.wisdom
      , { score := 14, modifier := 1, proficiencies :=
          { (.perception, .proficiency)
          , (.insight, .proficiency)
          }
        }
      ⟩
    , ⟨.charisma, {score := 12, modifier := 0, proficiencies := {(.deception, .proficiency)}}⟩
    }
  skillsHasAllAbilities := by
    intro (x : Ability)
    cases x with trivial
  feats := [.abilityImprovement .intelligence .intelligence]

#eval carryingCapacity melanie
