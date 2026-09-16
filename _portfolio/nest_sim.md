---
title: Nest survival bias simulation
permalink: /projects/nest-simulation/
github_repo: "smbolinger/sim_model"
link_page: true
preview_image: true
header:
  teaser: "assets/images/submodels.webp"
excerpt: “Virtual ecologist simulation of nest observation process. Used to examine effects of varying proportion of nest fates marked unknown (and excluded from DSR analysis) and proportion of nest fates misclassified.”
---

<!-- <figure style="width: 700px;"> -->
<figure style="float: right; max-width: 600px;">
    <img align="right" style="margin: 15px;" src="/assets/images/submodels1.webp" alt="depiction of the two submodels">
    <figcaption style="font-style: italic;"> Visualization of the two submodels.</figcaption>
</figure>

The model has two submodels:

1. The nest creation submodel. Randomly chooses initiation date from a real distribution of SW Louisiana nest initiation dates. Randomly samples a negative binomial distribution to get number of days until nest failure (probability = 1-DSR). If number of days exceeds incubation length, nest hatches.

2. The observer submodel. Ability to correctly classify nest fate decreases over time following a simple exponential decay function. 


<!-- <figure style="width: 700px;"> -->
<!--     <img align="right" style="margin: 15px;" src="/assets/images/sim_output.webp" alt="output of the python simulation"> -->
<!--     <figcaption style="font-style: italic;"> Output of the Python simulation.</figcaption> -->
<!-- </figure> -->
## Output from the Python simulation:

### Parameter values:

        stormFate stormUnk  pMortFl  MCtype  propMC  propUnk stormFrq stormDur decayRate  obsFreq hatchTime numNests probSurv discProb  brDays 
          "TRUE"   "TRUE"   "0.75"  "none"     "0"      "0"      "2"      "1"   "0.08"      "7"      "20"    "250"   "0.96"    "0.8"    "180" 

### Nest creation submodel:

        >> storm days =  59 63
        >> survey days, w/o storms:
            [1 8 15 22 29 36 43 50 57 66 73 80 87 94 101 108 115]
        >> survey intervals:      
            [0 7  7  7  7  7  7  7  7  9  7  7  7  7   7   7   7] ; sum(surveyInts>obsFreq)=1

        >> init dates:         
          [  25 8 47 32 43 32 61 73 43 30 26 46 43 58 11 52 11 36 27 20]
        >> survival in days:   
            [20 20 3 14 20 20 20 16 20 20 7 20 20 20 20 20 20 14 20 13]

        >> which storm?             
            [ 0 0 0 0 59 0 63 0 59 0 0 59 59 59 0 59 0 0 0 0]
        >> flooded?      
            [ 0 0 0 0 1 0 1 0 1 0 0 1 1 0 0 1 0 0 0 0] 
        >> end dates (w/storms):   
            [45 28 50 46 59 52 63 89 59 50 33 59 59 78 31 59 31 50 47 33]

        >> true final nest fate:  
            [ 0 0 1 1 2 0 2 1 2 0 1 2 2 0 0 2 0 1 0 1]

<figure style="width: 400px;">
    <img align="right" style="margin: 15px;" src="/assets/images/init_dates.webp" alt="sampled nest initiation dates for 100 runs of model compared to true initiation dates. the sampled distibutrions follow the true distribution.">
    <figcaption style="font-style: italic;"> Sampled nest initiation dates for 100 runs of model (pink) compared to true initiation dates (blue).</figcaption>
</figure>

### Observer submodel:

        >> surveys til discovery:
            [0 0 0 3 0 0 0 1 1 0 0 0 0 1 0 2 0 2 0 0]
        >> total num surveys: 
            [3 3 0 2 3 3 0 3 3 2 1 2 3 0 3 1 3 2 3 2]

        > nest discovered? (svysTilDiscovery < num_svy):
            [1 1 0 0 1 1 0 1 1 1 1 1 1 0 1 0 1 0 1 1]

        >> first found: 
            [ 29  8  0  0 43  36 0 80 50 36 29 50 43  0 15  0 15  0 29 22]
        >> last active:
            [ 50 29  0  0 57  57 0 87 57 50 29 57 57  0 36  0 36  0 50 29]
        >> last checked:
            [ 50 29  0  0 66  57 0 94 66 50 36 66 66  0 36  0 36  0 50 36]

        >> np.where(fateProb<fateCuesPresent):
            [2  3  4  5  7 11 15 16 17 18 ]

        >> np.where(stormFinalInt):    
            [4  6  8  11  12  13  15]

        >> assigned fate:     
            [7 7 1 1 2 0 2 1 2 7 7 2 2 2 7 2 0 1 0 7]


{% if project.github_repo %}
  <a href="https://github.com{{ project.github_repo }}" target="_blank">
    View on GitHub
  </a>
{% endif %}
