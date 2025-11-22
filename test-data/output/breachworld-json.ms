.\" text width
.nr LL 7i
.\" left margin
.nr PO 0.75i
.\" top margin
.nr HM 0.75i
.\" bottom margin
.nr FM 0.75i
.\" header/footer width
.nr LT \n[LL]
.\" point size
.nr PS 10p
.\" line height
.nr VS 12p
.\" font family: A, BM, H, HN, N, P, T, ZCM
.fam P
.\" paragraph indent
.nr PI 0m
.\" Quote indent
.nr QI 2n
.\" interparagraph space
.nr PD 0.5v
.\" footnote width
.nr FL \n[LL]
.\" footnote point size
.nr FPS (\n[PS] - 2000)
.\" footnote mode
.nr FF 3
.\" footnote length
.nr FL 3.4i
.\" color for links (rgb)
.ds PDFHREF.COLOUR   0.35 0.00 0.60
.\" border for links (default none)
.ds PDFHREF.BORDER   0 0 0
.\" point size difference between heading levels
.nr PSINCR 3p
.\" heading level above which point size no longer changes
.nr GROWPS 3
.\" page numbers in footer, centered
.rm CH
.ds CF %
.\" pdf outline fold level
.nr PDFOUTLINE.FOLDLEVEL 3
.\" start out in outline view
.pdfview /PageMode /UseOutlines
.hy
.\" ----------------------------------------------------------------------
.\" The title looks too small if we're using GROWPS, so adjust its size.
.\" 
.de TL
.br
.als TL cov*err-not-again
.rn @AB AB
.rn @AU AU
.rn @AI AI
.di cov*tl-div
.par@reset
.ft B
.nr tkb-psincr (\\n[PSINCR]*\\n[GROWPS])+2p
.ps +\\n[tkb-psincr]u
.vs +3p
.ll (u;\\n[LL]*5/6)
.nr cov*n-au 0
.DEVTAG-TL
..
.sp 1v
.LP
\fB\s[+6]Allosaur (Juvenile)\s0\fP
.LP
.KS
\fBScale:\fP +2D: PCs get +2D to hit and +6 to Dodge. It gets +2D to Damage and +6 to soak.
.br
\fBMight 3D+1\fP — Brawl 5D, Stamina 4D
.br
\fBAgility 3D+1\fP — Athletics (Run) 5D+1, Dodge 5D+2, Stealth 4D+2
.br
\fBWit 0D\fP
.br
\fBCharm 1D\fP
.br
\fBStatic:\fP Dodge 15, Soak 13 (19 vs. Scale +0D)
.br
\fBWL:\fP S:1–3 □ W:4–8 □ SW:4–8 □ I:9–12 □ MW:13–15 □
\fBMove:\fP 16
.br
\fBNatural Armor:\fP +3 Armor (included in Soak)
.br
\fBNatural Weapons:\fP +2D Teeth: 5D+1 damage (7D+1 vs Scale +0D
.br
.KE

.sp 1v
.LP
\fB\s[+6]Dragonfly\s0\fP
.LP
The creature is huge, as big as an elephant.  It is semi-humanoid, having the head and wings of a immense dragonfly, two pairs of arms, a pair of legs, and a segmented body part coming out of the base of its spine that ends in a long stinger.  It has three sets of mandibles for chewing.  It is covered by chitinous armor.  Its eyes glow blue, and its body is dark red on the head and torso, fading to a lighter red along its extremities.
.LP
.KS
\fBScale:\fP +2D: PCs get +2D to hit and +6 to Dodge. It gets +2D to Damage and +6 to soak.
.br
\fBMight 3D\fP — Brawl 5D, Stamina 4D
.br
\fBAgility 3D+1\fP — Athletics 5D+1, Dodge 5D
.br
\fBWit 2D\fP
.br
\fBCharm 1D\fP
.br
\fBStatic:\fP Dodge 15, Soak 15 (21 vs. Scale +0D)
.br
\fBWL:\fP S:1–3 □ W:4–8 □ SW:4–8 □ I:9–12 □ MW:13–15 □
\fBMove:\fP 15; Flying 25
.br
\fBNatural Armor:\fP +6 Chitin Armor (included in Soak)
.br
\fBNatural Weapons:\fP +2D Mandibles: 5D damage (7D vs. Scale +0D); +1D Stinger: 4D damage (6D vs. Scale +0D) with Poison (Resist with Stamina (or Might) instead of Soak), damage 5D; Successful hit by poison also requires a Moderate (11–15) Stamina check against the searing pain, which results in a penalty of –2D on all actions, in addition to any wounds.
.br
\fBSpecial Defenses:\fP Resistant to Lightning: ½ damage
.br
.KE

.sp 1v
.LP
\fB\s[+6]Imp, Yellow\s0\fP
.LP
.KS
\fBMight 2D\fP — Brawl 3D
.br
\fBAgility 5D\fP — Athletics 6D, Dodge 7D, Stealth 5D
.br
\fBWit 1D+2\fP
.br
\fBCharm 1D\fP
.br
\fBStatic:\fP Block 9, Dodge 21, Soak 6
.br
\fBWL:\fP S:1–3 □ W:4–8 □ SW:4–8 □ I:9–12 □ MW:13–15 □
\fBMove:\fP 11
.br
\fBSpecial Abilities:\fP May perform the Aether Feat of Spontaneous; Combustion once per hour as a natural ability.; Is immune to damage by fire and heat.
.br
.KE

.sp 1v
.LP
\fB\s[+6]Agi - Ruthless Machine Man Melee Merc\s0\fP
.LP
.KS
\fBMight 3D+2\fP — Axe 5D+2, Knife 5D+2
.br
\fBAgility 3D\fP — Handgun 5D
.br
\fBWit 4D\fP — Navigate 5D, Perception 5D, Survive 5D, Track 5D
.br
\fBCharm 1D+2\fP
.br
\fBPerks:\fP Quick Study, Recall
.br
\fBComplications:\fP Soulless
.br
\fBGear:\fP Battle Axe (+3D), Knife (+1D), Pistol (4D, 8 shots, 30/65/100 feet)
.br
\fBDefenses:\fP Block 9, Dodge 9, Parry 17 (axe or knife), Soak 11
.br
\fBWL:\fP S:1–3 □ W:4–8 □ SW:4–8 □ I:9–12 □ MW:13–15 □
\fBMove:\fP 15
.br
\fBMelee:\fP **Battle Axe:** Axe 5D+2, damage 6D+2; **Fists:** Might 3D, damage 3D; **Knife:** Knife 5D+2, damage 4D+2
.br
\fBRanged:\fP **Pistol:** Handgun 4D, damage 4D, 8 shots, 30/65/100 feet
.br
.KE
