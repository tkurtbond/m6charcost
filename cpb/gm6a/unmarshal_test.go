package main

import (
	"encoding/json"
	"reflect"
	"testing"
)

func TestSkillType(t *testing.T) {
	var data = [][]SkillType{
		{},
		{{Name: "A1", Dice: "3D"}},
		{{Name: "A2", Dice: "3D"}, {Name: "B2", Dice: "4D"}, {Name: "C2", Dice: "5D"}},
	}

	for _, check := range data {
		output, err := json.MarshalIndent(check, "", "  ")
		if err != nil {
			t.Fatal(err)
		}

		// fmt.Println(string(output))

		var skill []SkillType
		err = json.Unmarshal(output, &skill)
		if err != nil {
			t.Fatal(err)
		}

		// fmt.Printf("%+v\n", skill)

		if len(check) != len(skill) {
			t.Errorf("%d %d oops", len(check), len(skill))
		}
		for i := range check {
			if check[i].Name != skill[i].Name {
				t.Errorf("%d %s %s oops", i, check[i].Name, skill[i].Name)
			}
		}
	}
}

func TestStatisticType(t *testing.T) {
	var data = []StatisticType{
		{Dice: "3D"},
		{Dice: "4D", Skill: []SkillType{{Name: "A1", Dice: "3D"}}},
		{Dice: "5D", Skill: []SkillType{{Name: "A2", Dice: "3D"}, {Name: "B2", Dice: "4D"}, {Name: "C2", Dice: "5D"}}},
	}

	for _, check := range data {
		output, err := json.MarshalIndent(check, "", "  ")
		if err != nil {
			t.Fatal(err)
		}

		// fmt.Println(string(output))

		var stat StatisticType
		err = json.Unmarshal(output, &stat)
		if err != nil {
			t.Fatal(err)
		}

		// fmt.Printf("%+v", stat)

		if check.Dice != stat.Dice {
			t.Errorf("%s %s oops", check.Dice, stat.Dice)
		}
		if !reflect.DeepEqual(check.Skill, stat.Skill) {
			t.Errorf("%v %v oops", check.Skill, stat.Skill)
		}
	}
}

func TestCharacterType(t *testing.T) {
	var data = CharacterType{
		Name:          "name",
		Archetype:     "archetype",
		Description:   "description",
		Player:        "player",
		Might:         StatisticType{Dice: "4D", Skill: []SkillType{{Name: "A1", Dice: "3D"}}},
		Agility:       StatisticType{Dice: "4D", Skill: []SkillType{{Name: "A2", Dice: "3D"}}},
		Wit:           StatisticType{Dice: "4D", Skill: []SkillType{{Name: "A3", Dice: "3D"}}},
		Charm:         StatisticType{Dice: "4D", Skill: []SkillType{{Name: "A4", Dice: "3D"}}},
		Perks:         []SkillType{{Name: "A4", Dice: "3D"}},
		Complications: []string{"C1"},
		Gear:          []string{"C2", "C3"},
		HeroPoints:    1,
		Static:        "static",
	}

	output, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		t.Fatal(err)
	}

	// fmt.Println(string(output))

	var char CharacterType
	err = json.Unmarshal(output, &char)
	if err != nil {
		t.Fatal(err)
	}

	// fmt.Printf("%+v", char)

	if !reflect.DeepEqual(data, char) {
		t.Errorf("%v %v oops", data, char)
	}
}

func TestCharactersArray(t *testing.T) {
	var data = []CharacterType{
		CharacterType{
			Name:  "A",
			Might: StatisticType{Dice: "4D", Skill: []SkillType{{Name: "A1", Dice: "3D"}}},
		},
		CharacterType{
			Name:  "B",
			Might: StatisticType{Dice: "4D", Skill: []SkillType{{Name: "A2", Dice: "3D"}}},
		},
		CharacterType{
			Name:  "C",
			Might: StatisticType{Dice: "4D", Skill: []SkillType{{Name: "A3", Dice: "3D"}}},
		},
	}

	output, err := json.MarshalIndent(data, "", "  ")
	if err != nil {
		t.Fatal(err)
	}

	// fmt.Println(string(output))

	var chars []CharacterType
	err = json.Unmarshal(output, &chars)
	if err != nil {
		t.Fatal(err)
	}

	if !reflect.DeepEqual(data, chars) {
		t.Errorf("%v %v oops", data, chars)
	}
}
