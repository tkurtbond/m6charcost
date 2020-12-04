#! /usr/bin/env python3.7
# m6.py - calculate points costs of Mini-Six Characters in YAML.
import yaml
import re
import sys

class InvalidStat (Exception):
    def __init__ (self, stat, msg):
        self.stat = stat
        self.msg = msg

pips_per_die = 3
dice_rx = re.compile (r"^([0-9]+)[Dd](\+([1-2]))?$")

def dice_to_cost (stat):
    m = dice_rx.match (stat)
    if m == None:
        raise InvalidStat (stat, 'invalid stat')
    dice = int (m.group (1))
    pips = int (m.group (3)) if m.group (3) else 0
    return (dice * pips_per_die) + pips

def cost_to_dice (cost):
    dice = cost / pips_per_die
    pips = cost % pips_per_die
    return '%dd+%d' % (dice, pips)
        

f = open (sys.argv[1], 'r')
s = f.read ()

characters = yaml.safe_load (s)

# TODO: Figure out how to print out formatted characters.

num_characters = 0
for character in characters:
    num_characters += 1
    if num_characters > 1:
        print
        print ('=' * 43)
    if 'Name' in character:
        print ('name: %s' % character['Name'])
    if 'Description' in character:
        print ('      %s' % character['Description'])
    if 'Player' in character:
        print ('player: %s' % character['Player'])
    if 'Statistics' in character:
        statistics = character['Statistics']
    else:
        statistics = [ "Might", "Agility", "Wit", "Charm" ]
    # TODO: Check what form skills are listed, relative or absolute.
    # TODO: Check where skills listed, with stats or under 'skills'
    # 
    # This works for absolute skills listed with stats.
    total_stat_cost = 0
    total_skill_cost = 0
    for statname in statistics:
        stat_and_skills = character[statname]
        if len (stat_and_skills) < 1:
            raise Exception ('Missing stat: %d' % statname)
        stat = stat_and_skills[:1][0]
        stat_cost = dice_to_cost (stat)
        total_stat_cost += stat_cost
        print ('%-30s (%3d points)' % ('%s: %s' % (statname, stat), stat_cost))
        skills = stat_and_skills[1:]
        skills = sorted (skills, key=lambda skill: skill[0])
        for [name, dice] in skills:
             skill_cost = dice_to_cost (dice)
             relative_cost = skill_cost - stat_cost
             relative_dice = cost_to_dice (relative_cost)
             total_skill_cost += relative_cost
             print ('    %-19s %6s (%3d points)' % (
                 '%s: %s' % (name, dice),
                 '+' + relative_dice,
                 relative_cost))
    total_stat_dice = cost_to_dice (total_stat_cost)
    total_skill_dice = cost_to_dice (total_skill_cost)
    total_skill_and_perk_cost = total_skill_cost
    total_perk_cost = 0 
    if 'Perks' in character:
        print ('Perks: ')
        perk_count = 0
        for [perk, perk_dice] in character['Perks']:
            perk_cost = dice_to_cost (perk_dice)
            perk_count = perk_count + 1
            print ('    %-26s (%3d points)' % ("%s: %s" % (perk, perk_dice), perk_cost))
            # Perk cost is in dice
            total_perk_cost += perk_cost
            total_skill_and_perk_cost += perk_cost
    total_perk_dice = cost_to_dice (total_perk_cost)
    print
    print ('total stat:             %6s (%3d points)' % 
           (total_stat_dice, total_stat_cost))
    print ('total skill:            %6s (%3d points)' % 
           (total_skill_dice, total_skill_cost))
    print ('total perk:             %6s (%3d points)' %
           (total_perk_dice, total_perk_cost))
    total_skill_and_perk_dice = cost_to_dice (total_skill_and_perk_cost)
    print ('total (skill + perk):   %6s (%3d points)' %
           (total_skill_and_perk_dice, total_skill_and_perk_cost))
    total_cost = total_stat_cost + total_skill_cost + total_perk_cost
    total_dice = cost_to_dice (total_cost)
    print ('total:                  %6s (%3d points)' %
           (total_dice, total_cost))
