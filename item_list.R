prep_imbu_tab <- function(){
  require(data.table)
  require(stringdist)
  img_files <- list.files("www", pattern = "\\.gif")
  img_files <- substr(img_files, 1, nchar(img_files)-4)
  img_files <- gsub(img_files, pattern = "_", replacement = " ", fixed = T)
  
  item_list <- data.table(var = c('axe', 'axe', 'axe',
                                  'cap', 'cap', 'cap',
                                  'club', 'club', 'club',
                                  'crit', 'crit', 'crit',
                                  'death_dmg', 'death_dmg', 'death_dmg',
                                  'death_prot', 'death_prot', 'death_prot',
                                  'dist', 'dist', 'dist',
                                  'earth_dmg', 'earth_dmg', 'earth_dmg',
                                  'earth_prot', 'earth_prot', 'earth_prot',
                                  'energy_dmg', 'energy_dmg', 'energy_dmg',
                                  'energy_prot', 'energy_prot', 'energy_prot',
                                  'fire_dmg', 'fire_dmg', 'fire_dmg',
                                  'fire_prot', 'fire_prot', 'fire_prot',
                                  'holy_prot', 'holy_prot', 'holy_prot',
                                  'ice_dmg', 'ice_dmg', 'ice_dmg',
                                  'ice_prot', 'ice_prot', 'ice_prot',
                                  'life_leech', 'life_leech', 'life_leech',
                                  'mana_leech', 'mana_leech', 'mana_leech',
                                  'mlev', 'mlev', 'mlev',
                                  'paral_defl', 'paral_defl', 'paral_defl',
                                  'shield', 'shield', 'shield',
                                  'speed', 'speed', 'speed',
                                  'sword', 'sword', 'sword'),
                          name = c('Chop (Axe Fight.)', 'Chop (Axe Fight.)', 'Chop (Axe Fight.)',
                                   'Featherweight (Capacity)', 'Featherweight (Capacity)', 'Featherweight (Capacity)', 
                                   'Bash (Club Fight.)', 'Bash (Club Fight.)', 'Bash (Club Fight.)',
                                   'Strike (Crit)', 'Strike (Crit)', 'Strike (Crit)',
                                   'Reap (Death Dmg)', 'Reap (Death Dmg)', 'Reap (Death Dmg)',
                                   'Lich Shroud (Death Prot)', 'Lich Shroud (Death Prot)', 'Lich Shroud (Death Prot)',
                                   'Precision (Dist. Fight.)', 'Precision (Dist. Fight.)', 'Precision (Dist. Fight.)',
                                   'Venom (Earth Dmg)', 'Venom (Earth Dmg)', 'Venom (Earth Dmg)',
                                   'Snake Skin (Earth Prot)', 'Snake Skin (Earth Prot)', 'Snake Skin (Earth Prot)', 
                                   'Electrify (Energy Dmg)', 'Electrify (Energy Dmg)', 'Electrify (Energy Dmg)',
                                   'Cloud Fabric (Energy Prot)', 'Cloud Fabric (Energy Prot)', 'Cloud Fabric (Energy Prot)',
                                   'Scorch (Fire Dmg)', 'Scorch (Fire Dmg)', 'Scorch (Fire Dmg)',
                                   'Dragon Hide (Fire Prot)', 'Dragon Hide (Fire Prot)', 'Dragon Hide (Fire Prot)', 
                                   'Demon Presence (Holy Prot)', 'Demon Presence (Holy Prot)', 'Demon Presence (Holy Prot)',
                                   'Frost (Ice Dmg)', 'Frost (Ice Dmg)', 'Frost (Ice Dmg)',
                                   'Quara Scale (Ice Prot)', 'Quara Scale (Ice Prot)', 'Quara Scale (Ice Prot)',
                                   'Vampirism (Life Leech)', 'Vampirism (Life Leech)', 'Vampirism (Life Leech)',
                                   'Void (Mana Leech)', 'Void (Mana Leech)', 'Void (Mana Leech)',
                                   'Epiphany (Magic)', 'Epiphany (Magic)', 'Epiphany (Magic)', 
                                   'Vibrancy (Paralysis Defl.)', 'Vibrancy (Paralysis Defl.)', 'Vibrancy (Paralysis Defl.)',
                                   'Blockade (Shield)', 'Blockade (Shield)', 'Blockade (Shield)', 
                                   'Swiftness (Speed)', 'Swiftness (Speed)', 'Swiftness (Speed)', 
                                   'Slash (Sword Fight.)', 'Slash (Sword Fight.)', 'Slash (Sword Fight.)'),
                          category = c('skills', 'skills', 'skills', 
                                       'other', 'other', 'other', 
                                       'skills', 'skills', 'skills',
                                       'other', 'other', 'other', 
                                       'ele_dmg', 'ele_dmg', 'ele_dmg', 
                                       'ele_dmg', 'ele_dmg', 'ele_dmg',
                                       'skills', 'skills', 'skills', 
                                       'ele_dmg', 'ele_dmg', 'ele_dmg',
                                       'ele_prot', 'ele_prot', 'ele_prot', 
                                       'ele_dmg', 'ele_dmg', 'ele_dmg', 
                                       'ele_prot', 'ele_prot', 'ele_prot',
                                       'ele_dmg', 'ele_dmg', 'ele_dmg', 
                                       'ele_prot', 'ele_prot', 'ele_prot',
                                       'ele_prot', 'ele_prot', 'ele_prot',
                                       'ele_dmg', 'ele_dmg', 'ele_dmg',
                                       'ele_prot', 'ele_prot', 'ele_prot',
                                       'leech', 'leech', 'leech',
                                       'leech', 'leech', 'leech',
                                       'skills', 'skills', 'skills',
                                       'other', 'other', 'other', 
                                       'skills', 'skills', 'skills',
                                       'other', 'other', 'other', 
                                       'skills', 'skills', 'skills'),
                          stage = c(rep(1:3, 23)),
                          item = c('Orc Teeth', 'Battle Stones', 'Moohtant Horns',
                                   'Fairy Wings', 'Little Bowls of Myrrh', 'Goosebump Leather',
                                   'Cyclops Toes', 'Ogre Nose Rings', "Warmaster's Wristguards",
                                   'Protective Charms', 'Sabreteeth', 'Vexclaw Talons',
                                   'Piles of Grave Earth', 'Demonic Skeletal Hands', 'Petrified Screams',
                                   'Flasks of Embalming Fluid', 'Gloom Wolf Furs', 'Mystical Hourglasses',
                                   'Elven Scouting Glasses', 'Elven Hoofs', 'Metal Spikes',
                                   'Swamp Grass', 'Poisonous Slimes', 'Slime Hearts',
                                   'Pieces of Swampling Wood', 'Snake Skins', 'Brimstone Fangs',
                                   'Rorc Feathers', 'Peacock Feather Fans', 'Energy Vein',
                                   'Wyvern Talismans', 'Crawler Head Platings', 'Wyrm Scales',
                                   'Fiery Hearts', 'Green Dragon Scales', 'Demon Horns',
                                   'Green Dragon Leathers', 'Blazing Bones', 'Draken Sulphur', 
                                   'Cultish Robes', 'Cultish Masks', 'Hellspawn Tails', 
                                   'Frosty Hearts', 'Seacrest Hair', 'Polar Bear Paws',
                                   'Winter Wolf Furs', 'Thick Furs', 'Deepling Warts',
                                   'Vampire Teeth', 'Bloody Pincers', 'Piece of Dead Brain',
                                   'Rope Belts', 'Silencer Claws', 'Some Grimleech Wings',
                                   'Elvish Talismans', 'Broken Shamanic Staffs', 'Strands of Medusa Hair',
                                   'Wereboar Hooves', 'Crystallized Anger', 'Quills', 
                                   'Pieces of Scarab Shell', 'Brimstone Shells', 'Frazzle Skins',
                                   'Damselfly Wings', 'Compasses', 'Waspoid Wings',
                                   "Lion's Manes", "Mooh'tah Shells", 'War Crystals'),
                          number_of = c(20, 25, 20,
                                        20, 10, 5,
                                        20, 15, 10,
                                        20, 25, 5,
                                        25, 20, 5,
                                        25, 20, 5,
                                        25, 20, 10,
                                        25, 20, 2,
                                        25, 20, 10,
                                        25, 5, 1,
                                        20, 15, 10,
                                        25, 5, 5,
                                        20, 10, 5, 
                                        25, 25, 20, 
                                        25, 10, 5,
                                        25, 15, 10, 
                                        25, 15, 5,
                                        25, 25, 5,
                                        25, 15, 5,
                                        20, 15, 5,
                                        20, 25, 25,
                                        15, 25, 20,
                                        25, 25, 5),
                          price = rep(0,69))

  img_names_sort <- amatch(item_list$item, img_files, maxDist = 2)
  img_files <- img_files[img_names_sort]
  img_files <- gsub(img_files, pattern = "\\s",replacement = "_" )
  
  item_list <- cbind(item_list, img_files)  
}
