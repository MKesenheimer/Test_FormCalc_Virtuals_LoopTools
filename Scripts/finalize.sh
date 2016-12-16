#!/bin/bash
	gsed -i -e s/List\(//g bornmunu_ubaru_gamg.f
	gsed -i -e s/,TAG\)//g bornmunu_ubaru_gamg.f
	gsed -i -e 's/\t/      /g' bornmunu_ubaru_gamg.f
	mv bornmunu_ubaru_gamg.f bornmunu_ubaru_gamg.F
