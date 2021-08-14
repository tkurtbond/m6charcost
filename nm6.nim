import os
import json
import re
import sequtils
import strformat
import strutils
import sugar

type BadDiceError* = object of ValueError

let diceRe = re"^([0-9]+)[Dd](\+([1-2]))?$"

let pipsPerDie = 3

proc diceToCost(diceString: string): int =
  var matches: array[3, string]
  if not match(diceString, diceRe, matches):
    raise newException(BadDiceError, fmt"Bad Dice Value: '{diceString}'")
  (parseInt(matches[0]) * pipsPerDie) + (if matches[2].len == 0: 0
                                         else: parseInt(matches[2]))

proc costToDice(cost: int): string =
  let dice = cost div pipsPerDie
  let pips = cost mod pipsPerDie
  return fmt"{dice}D+{pips}"
  

proc calculateCharacter(character: JsonNode) =
  var
    totalStatCost = 0
    totalSkillCost = 0
    totalPerkCost = 0
    totalSkillAndPerkCost = 0
  let
    name = character["Name"].getStr()
    player = character["Player"].getStr()
    statistics = if character.hasKey("Statistics"):
                   character["Statistics"].getElems.map(x => x.getStr())
                 else:
                   @["Might", "Agility", "Wit", "Charm"]
  echo fmt"Name: {name}"
  if character.hasKey ("Description"):
    let desc = character["Description"].getStr()
    echo fmt"    {desc}"
  echo fmt"Player: {player}"
  for statName in statistics:
    let statArray = character[statName]
    let statValue = statArray[0].getStr()
    let statCost = diceToCost(statValue)
    totalStatCost += statCost
    let statString = fmt "{statName}: {statValue}"
    echo fmt"{statString:30} ({statCost:3} points)"
    for i in 1..statArray.len-1:
      let skillName = statArray[i][0].getStr()
      let skillValue = statArray[i][1].getStr()
      let skillCost = diceToCost(skillValue)
      let relativeCost = skillCost - statCost
      let relativeDice = "+" & costToDice(relativeCost)
      totalSkillCost += relativeCost
      totalSkillAndPerkCost += relativeCost
      let skillString = fmt"{skillName}: {skillValue}"
      echo fmt"    {skillString:19} {relativeDice:>6} ({relativeCost:3} points)"
  if character.hasKey("Perks"):
    echo "Perks:"
    for perk in character["Perks"]:
      let perkName = perk[0].getStr()
      let perkValue = perk[1].getStr()
      let perkCost = diceToCost(perkValue)
      totalPerkCost += perkCost
      totalSkillAndPerkCost += perkCost
      let perkString = fmt"   {perkName}: {perkValue}"
      echo fmt"{perkString:30} ({perkCost:3} points)"
  let totalStatDice = costToDice(totalStatCost)
  let totalSkillDice = costToDice(totalSkillCost)
  let totalPerkDice = costToDice(totalPerkCost)
  let totalSkillAndPerkDice = costToDice(totalSkillAndPerkCost)
  let tStat = "total stat:"
  echo &"{tStat:23} {totalStatDice:>6} ({totalStatCost:3} points)"
  let tSkill = "total skill:"
  echo &"{tSkill:23} {totalSkillDice:>6} ({totalSkillCost:3} points)"
  let tPerk = "total perk:"
  echo &"{tPerk:23} {totalPerkDice:>6} ({totalPerkCost:3} points)"
  let tSkillPlusPerk = "total (skill + perk):"
  echo &"{tSkillPlusPerk:23} {totalSkillAndPerkDice:>6} ({totalSkillAndPerkCost:3} points)"
  let totalCost = totalStatCost + totalSkillCost + totalPerkCost
  let totalDice = costToDice(totalCost)
  let  tTotal = "total:" 
  echo &"{tTotal:23} {totalDice:>6} ({totalCost:3} points)"

proc main() =
  if paramCount() != 1:
    quit("synopsis: " & getAppFilename() & " filename")
  let
    filename = paramStr (1)
    entireFile = readFile(filename)
  let characters = parseJson(entireFile)
  let lineOfEquals = '='.repeat(42)
  var i = 0
  for character in characters:
    inc i
    if i > 1: echo lineOfEquals
    calculate_character character

main()
