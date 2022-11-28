GraPhaRe: Gravitational Waves from Phase Transitions during Reheating
==============================================

Authors: Manuel A. Buen-Abad, Jae Hyeok Chang, Anson Hook

GraPhaRe
-----------------------------------

This code has been written in Wolfram language using Mathematica 12.0 by Manuel A. Buen-Abad and Jae Hyeok Chang (April 2022).



Installing the code
--------------

There are two packages: `PhaseTransition.wl`, which computes various observables from the first-order phase transition; and `DSReheating.wl`, which computes the reheating history of the dark sector.

To install, simply open a Mathematica notebook and:

1. Go to File -> Install.
2. In the "Source" drop menu, click "From File...".
3. Select the package.
4. Click "OK".


To call the packages, simply enter the following commands in a Mathematica cell:

    Needs["PhaseTransition`"]
    Needs["DSReheating`"]


Using the code
--------------

You can use `GraPhaRe` freely, provided that in your publications, you cite the paper [Buen-Abad, Chang, Hook (2022)](https://arxiv.org/abs/2212.xxxxx).

The BibTeX entry for the paper is:

    @article{Buch:2020xyt,
		author = "Buen-Abad, Manuel A. and Chang, Jae Hyeok and Hook, Anson",
		title = "{Gravitational Waves from Reheating}",
		eprint = "2212.xxxxx",
		archivePrefix = "arXiv",
		primaryClass = "hep-ph",
		doi = "10.1103/PhysRevD.102.083010",
		journal = "Phys. Rev. D",
		volume = "102",
		number = "8",
		pages = "083010",
		year = "2020"
	}
