#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 21 14:10:47 2021

@author: ryanswope
"""
tourney = [1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15]

def odds(x,y):
    if x>y:
        return 1-(x/(x+y))
    else:
        return y/(x+y)

def seed_odds(tourney, seed):   
    team_ind = tourney.index(seed)
    rounds = [2,4,8,16]
    #rounds = [2,4]
    round_odds = [[0 for t in tourney] for r in rounds]
    
    for r in range(len(rounds)):
        
        groups = len(tourney)/rounds[r]
        
        for i in range(int(groups)):
            teams = tourney[i*rounds[r]:(i+1)*rounds[r]]
            #print(teams)
            for t in range(len(teams)):
                odds_to_advance = 0
                #print(teams[t])
                if t<len(teams)/2:
                    teams_ = teams[int(len(teams)/2):]
                else:
                    teams_ = teams[:int(len(teams)/2)]
                for t_ in range(len(teams_)):
                    if teams[t] != teams_[t_]:
                        #print('odds vs {}: '.format(teams_[t_])+ str(odds(teams[t],teams_[t_])))
                        if rounds[r]==2:
                            odds_to_advance+=odds(teams[t],teams_[t_])
                        else:
                            #print('odds from last round: {}'.format(round_odds[r-1][tourney.index(teams_[t_])]))
                            odds_to_advance+=odds(teams[t],teams_[t_])*round_odds[r-1][tourney.index(teams_[t_])]
                if rounds[r]>2:
                    odds_to_advance *= round_odds[r-1][tourney.index(teams[t])]
                round_odds[r][i*rounds[r]+t] = odds_to_advance
                
    return round_odds[-1][team_ind]
                
                
#Baseline Odds
baseline = original = seed_odds(tourney,2)

print('Two seed odds to win default tournament: {}%'.format(round(100*original,5)))
#Iterate through possible tournament configs
for first in range(len(tourney)-2):
    for second in range(first+2,len(tourney)):
        new_tourney = [1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15]
        temp = tourney[first]
        new_tourney[first] = new_tourney[second]
        new_tourney[second] = temp
        
        new_odds = seed_odds(new_tourney,2)
        if new_odds>baseline:
            swap = new_tourney
            baseline = new_odds
print('Best configuration: {}'.format(swap))
print('New odds to win: {}%, an increase of {}%'.format(round(100*baseline,5),round(100*baseline,5)-round(100*original,5)))
print('The odds of the first seed winning decreased by {}% to {}%'.format(round(100*(seed_odds(tourney,1)-seed_odds(new_tourney,1)), 5), round(100*seed_odds(tourney,1),5)))


            
    