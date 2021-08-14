package main

import "encoding/json"

// ========================================

type SkillType struct {
	Name string
	Dice string
}

func (s *SkillType) UnmarshalJSON(b []byte) error {
	var raw []string
	err := json.Unmarshal(b, &raw)
	if err != nil {
		return err
	}

	// TODO: Assert length of raw?
	s.Name = raw[0]
	s.Dice = raw[1]
	return nil
}

func (s SkillType) MarshalJSON() ([]byte, error) {
	return json.Marshal([]string{s.Name, s.Dice})
}

// ========================================

type StatisticType struct {
	Dice string

	Skill []SkillType
}

func (s *StatisticType) UnmarshalJSON(b []byte) error {
	var raw []json.RawMessage
	err := json.Unmarshal(b, &raw)
	if err != nil {
		return err
	}

	// TODO: Assert len of raw?
	err = json.Unmarshal(raw[0], &s.Dice)
	if err != nil {
		return err
	}

	for _, v := range raw[1:len(raw)] {
		var skill SkillType
		err := json.Unmarshal(v, &skill)
		if err != nil {
			return err
		}
		s.Skill = append(s.Skill, skill)
	}
	return nil
}

func (s StatisticType) MarshalJSON() ([]byte, error) {
	rtn := make([]interface{}, 0, 10)
	rtn = append(rtn, s.Dice)
	for _, v := range s.Skill {
		rtn = append(rtn, v)
	}
	return json.Marshal(rtn)
}

type CharacterType struct {
	Name          string
	Archetype     string        `json:"Archetype,omitempty"`
	Description   string        `json:"Description,omitempty"`
	Player        string        `json:"Player,omitempty"`
	Might         StatisticType `json:"Might,omitempty"`
	Agility       StatisticType `json:"Agility,omitempty"`
	Wit           StatisticType `json:"Wit,omitempty"`
	Charm         StatisticType `json:"Charm,omitempty"`
	Perks         []SkillType   `json:"Perks,omitempty"`
	Complications []string      `json:"Complications,omitempty"`
	Gear          []string      `json:"Gear,omitempty"`
	HeroPoints    int           `json:"Hero_Points,omitempty"`
	Static        string        `json:"Static,omitempty"`
}
