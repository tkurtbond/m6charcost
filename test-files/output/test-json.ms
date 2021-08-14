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
.fam EBGaramond
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
.KS
.LP
\fB\s[+6]Morwen Polglaze — ruin/cave explorer (Lily Bond)\s0\fP
.LP
Very short, **very** muscular.  Murderous. :-)
.LP
\fBMight 4D\fP — Axe 6D
.br
\fBAgility 3D\fP — Dodge 5D, Crossbow 4D
.br
\fBWit 3D\fP — Search 4D, Survival 4D
.br
\fBCharm 2D\fP
.br
\fBStatic:\fP Dodge 15, Block 12, Parry 18 (Axe), Soak 12(18)
.br
\fBPerks:\fP Perceptive 2D, Recall 1D
.br
\fBGear:\fP Axe (+3D), Chainmail (+6), Crossbow (4D+1, 50/150/400, 20 bolts), Knife (+1D)
.br
\fBHero Points:\fP 2
.br
\fBWL:\fP S:1–3 □ W:4–8 □ SW:4–8 □ I:9–12 □ MW:13–16 □
.KE
.sp 1v
.KS
.LP
\fB\s[+6]Bersaba Kitto — elegant sorcerer (Lily Bond)\s0\fP
.LP
Tall, willowy, elegant, and abstracted.
.LP
\fBMight 3D\fP — Sword 5D
.br
\fBAgility 3D\fP — Dodge 5D, Sling 4D
.br
\fBWit 4D\fP — Magic 6D
.br
\fBCharm 2D\fP
.br
\fBStatic:\fP Dodge 15, Block 9, Parry 15 (Sword), Soak 9(15))
.br
\fBPerks:\fP Sorcerer 3D
.br
\fBSpells:\fP Fly, Hasten, See Magic Aura, Shape Change
.br
\fBGear:\fP Sword (+2D+2), Chainmail (+6), Elegant clothes, Knife (+1D), Sling (+2D+1, 40/130/400), 48 lead sling bullets
.br
\fBHero Points:\fP 2
.br
\fBWL:\fP S:1–3 □ W:4–8 □ SW:4–8 □ I:9–12 □ MW:13–16 □
.KE
