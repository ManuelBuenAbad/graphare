GraPhaRe: Gravitational Waves from Phase Transitions during Reheating
==============================================

Authors: Manuel A. Buen-Abad, Jae Hyeok Chang, Anson Hook

GraPhaRe
-----------------------------------

This code has been written in Wolfram language using Mathematica 12.0 by Manuel A. Buen-Abad and Jae Hyeok Chang (2022).



Installing the code
--------------

There are two packages: `DSReheating.wl`, which computes the reheating history of the dark sector; and `PhaseTransition.wl`, which computes various observables from the first-order phase transition.

To install, simply open a Mathematica notebook and:

1. Go to File -> Install.
2. In the "Source" drop menu, click "From File...".
3. Select the package.
4. Click "OK".


To call the packages, simply enter the following commands in a Mathematica cell:

    Needs["DSReheating`"]
    Needs["PhaseTransition`"]


Using the code
--------------

You can use `GraPhaRe` freely, provided that in any resulting publications you cite the paper [Buen-Abad, Chang, Hook (2023)](https://arxiv.org/abs/2305.09712).

The BibTeX entry for the paper is:

    @article{Buen-Abad:2023hex,
    author = "Buen-Abad, Manuel A. and Chang, Jae Hyeok and Hook, Anson",
    title = "{Gravitational wave signatures from reheating}",
    eprint = "2305.09712",
    archivePrefix = "arXiv",
    primaryClass = "hep-ph",
    doi = "10.1103/PhysRevD.108.036006",
    journal = "Phys. Rev. D",
    volume = "108",
    number = "3",
    pages = "036006",
    year = "2023"
    }
