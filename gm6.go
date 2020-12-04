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

var diceRx *regexp.Regexp

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

func calculateCharacter(character map[string]interface{}) {
	var totalStatCost = 0
	var totalSkillCost = 0
	var totalPerkCost = 0
	var totalSkillAndPerkCost = 0
	name := character["Name"].(string)
	fmt.Printf("Name: %s\n", name)
	desc := character["Description"].(string)
	fmt.Printf("    %s\n", desc)
	player := character["Player"].(string)
	fmt.Printf("Player: %s\n", player)
	statisticsJson, found := character["Statistics"].([]interface{})
	var statistics []string
	if found {
		statistics = make([]string, len(statisticsJson))
		for i, si := range statisticsJson {
			statistics[i] = si.(string)
		}
	} else {
		statistics = []string{"Might", "Agility", "Wit", "Charm"}
	}
	for _, statName := range statistics {
		statValue := character[statName].([]interface{})
		statDice := statValue[0].(string)
		statCost := diceToCost(statDice)
		totalStatCost += statCost
		fmt.Printf("%-30s (%3d points)\n",
			fmt.Sprintf("%s: %s", statName, statDice), statCost)
		skills := statValue[1:]
		for _, skill := range skills {
			skillName := skill.([]interface{})[0].(string)
			skillDice := skill.([]interface{})[1].(string)
			skillCost := diceToCost(skillDice)
			relativeCost := skillCost - statCost
			relativeDice := costToDice(relativeCost)
			totalSkillCost += relativeCost
			totalSkillAndPerkCost += relativeCost
			fmt.Printf("    %-19s %6s (%3d points)\n",
				skillName+": "+skillDice,
				"+"+relativeDice, relativeCost)
		}
	}
	perksJson, found := character["Perks"].([]interface{})
	if found {
		fmt.Printf("Perks:\n")
		for _, perkAndCost := range perksJson {
			perkName := perkAndCost.([]interface{})[0].(string)
			perkDice := perkAndCost.([]interface{})[1].(string)
			perkCost := diceToCost(perkDice)
			totalPerkCost += perkCost
			totalSkillAndPerkCost += perkCost
			fmt.Printf("    %-26s (%3d points)\n",
				fmt.Sprintf("%s: %s", perkName, perkDice),
				perkCost)
		}
	}
	totalStatDice := costToDice(totalStatCost)
	totalSkillDice := costToDice(totalSkillCost)
	totalPerkDice := costToDice(totalPerkCost)
	totalSkillAndPerkDice := costToDice(totalSkillAndPerkCost)

	fmt.Printf("%-23s %6s (%3d points)\n", "total stat:", totalStatDice,
		totalStatCost)
	fmt.Printf("%-23s %6s (%3d points)\n", "total skill:", totalSkillDice,
		totalSkillCost)
	fmt.Printf("%-23s %6s (%3d points)\n", "total perk:", totalPerkDice,
		totalPerkCost)
	fmt.Printf("%-23s %6s (%3d points)\n", "total (skill + perk):",
		totalSkillAndPerkDice, totalSkillAndPerkCost)

	totalCost := totalStatCost + totalSkillCost + totalPerkCost
	totalDice := costToDice(totalCost)
	fmt.Printf("%-23s %6s (%3d points)\n", "total:", totalDice, totalCost)
}

func processFile(filename string) {
	dat, err := ioutil.ReadFile(filename)
	check(err)
	var characters []map[string]interface{}
	json.Unmarshal([]byte(dat), &characters)
	for i, character := range characters {
		if i > 0 {
			fmt.Println(separatorLine)
		}
		calculateCharacter(character)
	}
}

func main() {
	rx, err := regexp.Compile("^([0-9]+)[Dd](\\+([1-2]))?$")
	if err != nil {
		panic(err)
	}
	diceRx = rx
	flag.Parse()
	for i, s := range flag.Args() {
		if i > 0 {
			fmt.Println(separatorLine)
		}
		processFile(s)
	}
}
