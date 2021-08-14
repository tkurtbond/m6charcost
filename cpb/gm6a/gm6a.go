package main

import (
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
)

const separatorLine = "==========================================="

func check(e error) {
	if e != nil {
		panic(e)
	}
}

var diceRx = regexp.MustCompile("^([0-9]+)[Dd](\\+([1-2]))?$")

const pipsPerDie = 3

func diceToCost(diceString string) int {
	res := diceRx.FindStringSubmatch(diceString)
	if len(res) != 4 {
		panic(errors.New(fmt.Sprintf("diceToCost: wrong number of groups: %d", len(res))))
	}
	var dice int
	var err error
	dice, err = strconv.Atoi(res[1])
	if err != nil {
		panic(err)
	}
	var pips = 0
	if res[3] != "" {
		pips, err = strconv.Atoi(res[3])
		if err != nil {
			panic(err)
		}
	}
	return (dice * pipsPerDie) + pips
}

func costToDice(cost int) string {
	dice := cost / pipsPerDie
	pips := cost % pipsPerDie
	return fmt.Sprintf("%dD+%d", dice, pips)
}

type CostType struct {
	TotalStat         int
	TotalSkill        int
	TotalPerk         int
	TotalSkillAndPerk int
}

func calculateStatistic(cost *CostType, name string, stat StatisticType) {
	if stat.Dice != "" {
		statCost := diceToCost(stat.Dice)
		cost.TotalStat += statCost
		fmt.Printf("%-30s (%3d points)\n", fmt.Sprintf("%s: %s", name, stat.Dice), statCost)

		for _, skill := range stat.Skill {
			skillCost := diceToCost(skill.Dice)
			relativeCost := skillCost - statCost
			relativeDice := costToDice(relativeCost)
			cost.TotalSkill += relativeCost
			cost.TotalSkillAndPerk += relativeCost
			fmt.Printf("    %-19s %6s (%3d points)\n", skill.Name+": "+skill.Dice, "+"+relativeDice, relativeCost)
		}
	}
}

func calculatePerks(cost *CostType, perks []SkillType) {
	if perks == nil {
		return
	}
	fmt.Printf("Perks:\n")
	for _, perk := range perks {
		perkCost := diceToCost(perk.Dice)
		cost.TotalPerk += perkCost
		cost.TotalSkillAndPerk += perkCost
		fmt.Printf("    %-26s (%3d points)\n", fmt.Sprintf("%s: %s", perk.Name, perk.Dice), perkCost)
	}
}

func calculateCharacter(character CharacterType) {
	var cost CostType

	fmt.Printf("Name: %s\n", character.Name)
	if character.Description != "" {
		fmt.Printf("    %s\n", character.Description)
	}
	fmt.Printf("Player: %s\n", character.Player)

	calculateStatistic(&cost, "Might", character.Might)
	calculateStatistic(&cost, "Agility", character.Agility)
	calculateStatistic(&cost, "Wit", character.Wit)
	calculateStatistic(&cost, "Charm", character.Charm)
	calculatePerks(&cost, character.Perks)

	totalStatDice := costToDice(cost.TotalStat)
	totalSkillDice := costToDice(cost.TotalSkill)
	totalPerkDice := costToDice(cost.TotalPerk)
	totalSkillAndPerkDice := costToDice(cost.TotalSkillAndPerk)

	fmt.Printf("%-23s %6s (%3d points)\n", "total stat:", totalStatDice, cost.TotalStat)
	fmt.Printf("%-23s %6s (%3d points)\n", "total skill:", totalSkillDice, cost.TotalSkill)
	fmt.Printf("%-23s %6s (%3d points)\n", "total perk:", totalPerkDice, cost.TotalPerk)
	fmt.Printf("%-23s %6s (%3d points)\n", "total (skill + perk):", totalSkillAndPerkDice, cost.TotalSkillAndPerk)

	totalCost := cost.TotalStat + cost.TotalSkill + cost.TotalPerk
	totalDice := costToDice(totalCost)
	fmt.Printf("%-23s %6s (%3d points)\n", "total:", totalDice, totalCost)
}

func loadFile(filename string) ([]CharacterType, error) {
	data, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}

	var characters []CharacterType
	json.Unmarshal([]byte(data), &characters)
	return characters, nil
}

func processFile(filename string) {
	characters, err := loadFile(filename)
	if err != nil {
		panic(err)
	}
	for i, c := range characters {
		if i > 0 {
			fmt.Println(separatorLine)
		}
		calculateCharacter(c)
	}
}

func main() {
	flag.Parse()
	for i, s := range flag.Args() {
		if i > 0 {
			fmt.Println(separatorLine)
		}
		processFile(s)
	}
}
