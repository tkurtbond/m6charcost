Todo for my Mini Six character costing program
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

• Read all the Mini Six products (including Exilium and Esoterica and
  Summerland) and compare the formats for character and NPC stats.
• sm6*.scm: Figure out the order things should be in, especially with
  respect to things I've added.  (Melee, Ranged, Natural Weapons,
  Special Abilities, etc.)
• sm6troff-ms
  - Set the header level like I set the underlines in sm6rst?
  - Add option to set the font family?
• ms6rst.scm sm6troff-ms.scm:
  - ✓Add ability to print in NPC format, with Attributes separate,
    followed by Skills.

    Change the "(do-list stat-name statistics ...)" into two different
    procedures. 
  - Pending on presence of flag in character?
• Make "(default ON)" into::

  (string-append "(currently " (on-or-off output-notes) ")
• Calculate Static/Defenses?
  Save attribute and value in hash.  Save skill and value in hash.
  Command line option to specify attribute/skill for Dodge.

  How do I know what attribute/skill to use for parry?
• Make them work for relative skills?
• Make them work for no attributes?
• Write a GUI character generator?  Probably javascript on the web?
• When no filenames are specified read stdin.
