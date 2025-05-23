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
\fB\s[+6]J'Lee - Bounty Hunter (Eva Beth Atha)\s0\fP
.LP
One side of head is shaved.
.LP
.KS
\fBMight 3D\fP — Brawling 4D, Melee 4D
.br
\fBAgility 4D\fP — Dodge 5D, Piloting 5D, Rifle 6D+1
.br
\fBWit 3D\fP — Computer 4D, Gunnery 3D+1, Navigation 5D, Search 4D, Tracking 4D+1
.br
\fBCharm 2D\fP — Diplomacy 2D+1
.br
\fBPerks:\fP Favors
.br
\fBComplications:\fP TBA (=ship)
.br
\fBHero Points:\fP 1
.br
\fBGear:\fP rifle (6D), vibroknife, body armor (+6), clothes, glowrod, macro binoculars
.br
\fBStatic:\fP Dodge 15, Block 12, Parry 19 (rifle), Soak 9+6=15
.br
\fBWL:\fP S:1–3 □ W:4–8 □ SW:4–8 □ I:9–12 □ MW:13–15 □
.KE

.sp 1v
.LP
\fB\s[+6]Havan Zul - Smuggler (Tristan Atha)\s0\fP
.LP
.KS
\fBMight 3D\fP — Brawling 4D
.br
\fBAgility 3D+2\fP — Dodge 5D+1, Piloting 5D+2, Pistol 5D+2, Stealth 4D+2
.br
\fBWit 3D+1\fP — Gunnery 5D+1, Navigation 5D, Repair 4D, Shields 3D+2
.br
\fBCharm 2D\fP — Streetwise 3D
.br
\fBComplications:\fP Large debt (=ship)
.br
\fBHero Points:\fP 1
.br
\fBGear:\fP tool kit, flight suit (+2), 2 blaster pistols (5D), macro binoculars, glowrod, body armor (+6)
.br
\fBStatic:\fP Dodge 16, Block 12, Parry 17 (pistol), Soak 9+6=15
.br
\fBWL:\fP S:1–3 □ W:4–8 □ SW:4–8 □ I:9–12 □ MW:13–15 □
.KE

.sp 1v
.LP
\fB\s[+6]Miagro - Cyborg (Mason Atha)\s0\fP
.LP
.KS
\fBMight 4D+1\fP
.br
\fBAgility 3D+2\fP — Dodge 4D, Pistol 4D+1, Stealth 5D+1
.br
\fBWit 2D\fP — Computer 4D+1, Demolition 3D+2, Pick Locks 3D, Security 4D+1
.br
\fBCharm 2D\fP — Streetwise 3D+1
.br
\fBPerks:\fP Cyborg
.br
\fBHero Points:\fP 1
.br
\fBGear:\fP pistol, lock pick, flight suit (+2), clothes, glowrod, macro binoculars, 2 grenades (1 smoke), body armor (+6)
.br
\fBStatic:\fP Dodge 12 Block 13, Parry 13, Soak 13+6=19
.br
\fBWL:\fP S:1–3 □ W:4–8 □ SW:4–8 □ I:9–12 □ MW:13–15 □
.KE

.sp 1v
.LP
\fB\s[+6]Anna Nemus - Jedi (Lily Bond)\s0\fP
.LP
.KS
\fBMight 4D\fP — Plasma Sword 6D
.br
\fBAgility 3D\fP — Pistol 5D, Stealth 5D
.br
\fBWit 3D\fP — History 4D, Magic 6D+1
.br
\fBCharm 2D\fP
.br
\fBPerks:\fP Sorcerer
.br
\fBPowers:\fP bless, charm, illusions, paralysis, sense danger (p. 31)
.br
\fBHero Points:\fP 1
.br
\fBGear:\fP plasma sword (+4D), blaster pistol (5D), flight suit (+2), clothes, glowrod, macro binoculars, body armor (+6)
.br
\fBStatic:\fP Dodge 9, Block 12, Parry 18 (lightsaber), Soak 12+6=18
.br
\fBWL:\fP S:1–3 □ W:4–8 □ SW:4–8 □ I:9–12 □ MW:13–15 □
.KE
